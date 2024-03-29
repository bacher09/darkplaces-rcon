module DarkPlaces.Rcon (
    RconMode(..),
    ProtocolOptions(..),
    RconInfo(..),
    RconConnection,
    defaultRcon,
    makeRcon,
    connect,
    close,
    send,
    recv,
    enableLog,
    disableLog,
    setPassword,
    setMode,
    setTimeDiff,
    getPassword,
    getMode,
    getTimeDiff,
    getHost,
    getPort,
    getChallengeTimeout,
    getChallengeRetries,
    setChallengeTimeout,
    setChallengeRetries
) where
import DarkPlaces.Rcon.Internal
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Network.Socket hiding (connect, close)
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB
import qualified Network.Socket.ByteString.Lazy as NBL
import Data.IORef
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad
import Control.Concurrent
import Data.Function
import System.Timeout
import Control.Exception


data RconMode = NonSecureRcon
              | TimeSecureRcon
              | ChallengeSecureRcon
    deriving(Show, Read, Eq, Ord, Enum, Bounded)


data ProtocolOptions = OnlyIPv4
                     | OnlyIPv6
                     | BothProtocols
    deriving(Show, Read, Eq, Ord, Enum, Bounded)


data RconInfo = RconInfo {
    rconHost             :: HostName,
    rconPort             :: ServiceName,
    rconMode             :: RconMode,
    rconPassword         :: B.ByteString,
    rconTimeDiff         :: Int,
    rconProtoOpt         :: ProtocolOptions,
    rconChallengeRetries :: Int,
    rconChallengeTimeout :: Int
} deriving (Show, Read, Eq)


data RconConnection = RconConnection {
    connSocket     :: Socket,
    connInfo       :: IORef RconInfo,
    connChallengeM :: MVar B.ByteString,
    connReceivedC  :: Chan B.ByteString,
    connReceiveTid :: ThreadId
}


defaultRcon :: RconInfo
defaultRcon = RconInfo {rconHost="localhost",
                        rconPort="26000",
                        rconMode=TimeSecureRcon,
                        rconPassword=B.empty,
                        rconTimeDiff=0,
                        rconProtoOpt=BothProtocols,
                        rconChallengeRetries=3,
                        rconChallengeTimeout=500000}


createDPSocket :: HostName -> ServiceName -> ProtocolOptions -> IO (Socket, SockAddr)
createDPSocket host port opt = do
    host_addr <- getHostAddr
    sock <- socket (addrFamily host_addr) Datagram (addrProtocol host_addr)
    N.connect sock (addrAddress host_addr)
    return (sock, addrAddress host_addr)
  where
    getHostAddr = head `fmap` getAddrInfo (Just hints) (Just host) (Just port)
    dg_hints = defaultHints {addrSocketType=Datagram, addrFlags=[AI_ADDRCONFIG]}
    hints = case opt of
        BothProtocols -> dg_hints
        OnlyIPv4 -> dg_hints {addrFamily=AF_INET}
        OnlyIPv6 -> dg_hints {addrFamily=AF_INET6}

-- | Create `RconInfo` for givven host, port and password
makeRcon :: HostName -> ServiceName -> B.ByteString -> RconInfo
makeRcon host port passw = defaultRcon {rconHost=host,
                                        rconPort=port,
                                        rconPassword=passw}


-- | Connect to darkplaces server
connect :: RconInfo -> IO RconConnection
connect rcon = do
    (sock, addr) <- createDPSocket host port opt
    info_ref <- newIORef rcon
    challengeM <- newEmptyMVar
    receivedC <- newChan
    tid <- forkIO $ fix $ \loop -> processConnection sock addr challengeM receivedC >> loop
    return RconConnection {
        connSocket=sock,
        connInfo=info_ref,
        connChallengeM=challengeM,
        connReceivedC=receivedC,
        connReceiveTid=tid
    }
  where
    host = rconHost rcon
    port = rconPort rcon
    opt = rconProtoOpt rcon
    processConnection sock addr challenge received = do
        (respData, respAddr) <- NB.recvFrom sock maxPacketSize
        let updateChallenge = mapM_ (tryPutMVar challenge) (parseChallenge respData)
        let updateData = mapM_ (writeChan received) (parseRcon respData)
        when (respAddr == addr) (updateChallenge >> updateData)


-- | Close connection
close :: RconConnection -> IO ()
close rcon = do
    finally (N.close $ connSocket rcon) (killThread $ connReceiveTid rcon)


getChallenge :: RconConnection -> IO (Maybe B.ByteString)
getChallenge rcon = do
    info <- readIORef $ connInfo rcon
    retry (rconChallengeRetries info) (rconChallengeTimeout info) unreliableChallenge
  where
    retry count tval func
        | count > 0 = do
            val <- timeout tval func
            case val of
                Just _  -> return val
                Nothing -> retry (count - 1) tval func
        | otherwise = return Nothing
    unreliableChallenge = do
        void $ NB.send (connSocket rcon) challengePacket
        takeMVar $ connChallengeM rcon


-- | Sends rcon command via `RconConnection`. Returns True if command was sent.
send :: RconConnection -> B.ByteString -> IO Bool
send conn command = do
    maybePacket <- rconPacket
    case maybePacket of
        Just packet -> NBL.sendAll (connSocket conn) packet >> return True
        Nothing -> return False
  where
    rconNonsec rcon = rconNonSecurePacket (rconPassword rcon) command
    rconSecTime rcon = do
        cur_time <- getPOSIXTime
        let send_time = realToFrac cur_time + fromIntegral (rconTimeDiff rcon) :: Double
        return $ rconSecureTimePacket send_time (rconPassword rcon) command
    rconSecChallenge rcon = do
        maybeChallenge <- getChallenge conn
        case maybeChallenge of
            Just challenge -> return $ Just $ rconSecureChallengePacket challenge (rconPassword rcon) command
            Nothing -> return Nothing
    rconPacket = do
        rcon <- readIORef $ connInfo conn
        case rconMode rcon of
            NonSecureRcon -> return $ Just $ rconNonsec rcon
            TimeSecureRcon -> Just <$> rconSecTime rcon
            ChallengeSecureRcon -> rconSecChallenge rcon


-- | Waits for rcon packet and return parsed response.
-- if parsing fails it discard packet and waits another.
recv :: RconConnection -> IO B.ByteString
recv = readChan . connReceivedC


socketStr :: RconConnection -> IO B.ByteString
socketStr c = BC.pack . show <$> getSocketName (connSocket c)


enableLogStr :: RconConnection -> IO B.ByteString
enableLogStr rc = BC.append log_begin <$> socketStr rc
  where
    log_begin = BC.pack "sv_cmd addtolist log_dest_udp "


disableLogStr :: RconConnection -> IO B.ByteString
disableLogStr rc = BC.append log_begin <$> socketStr rc
  where
    log_begin = BC.pack "sv_cmd removefromlist log_dest_udp "


-- | Send rcon command for activating rcon log.
-- This feature will not work over NAT
enableLog :: RconConnection -> IO Bool
enableLog c = send c =<< enableLogStr c

-- | Opposite action for `enableLog`
disableLog :: RconConnection -> IO Bool
disableLog c = send c =<< disableLogStr c


setParam :: RconConnection -> (RconInfo -> RconInfo) -> IO ()
setParam c f = atomicModifyIORef' (connInfo c) $ \r -> (f r, ())


getParam :: RconConnection -> (RconInfo -> a) -> IO a
getParam c fun = fun `fmap` readIORef (connInfo c)


setPassword :: RconConnection -> B.ByteString -> IO ()
setPassword c passw = setParam c $ \rcon -> rcon {rconPassword=passw}


setMode :: RconConnection -> RconMode -> IO ()
setMode c mode = setParam c $ \rcon -> rcon {rconMode=mode}


setTimeDiff :: RconConnection -> Int -> IO ()
setTimeDiff c time = setParam c $ \rcon -> rcon {rconTimeDiff=time}


getPassword :: RconConnection -> IO B.ByteString
getPassword c = getParam c rconPassword


getMode :: RconConnection -> IO RconMode
getMode c = getParam c rconMode


getTimeDiff :: RconConnection -> IO Int
getTimeDiff c = getParam c rconTimeDiff


getHost :: RconConnection -> IO HostName
getHost c = getParam c rconHost


getPort :: RconConnection -> IO ServiceName
getPort c = getParam c rconPort


setChallengeTimeout :: RconConnection -> Int -> IO ()
setChallengeTimeout c t = setParam c $ \rcon -> rcon {rconChallengeTimeout=t}


getChallengeTimeout :: RconConnection -> IO Int
getChallengeTimeout c = getParam c rconChallengeTimeout


setChallengeRetries:: RconConnection -> Int -> IO ()
setChallengeRetries c t = setParam c $ \rcon -> rcon {rconChallengeRetries=t}


getChallengeRetries:: RconConnection -> IO Int
getChallengeRetries c = getParam c rconChallengeRetries
