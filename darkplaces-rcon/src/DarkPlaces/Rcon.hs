module DarkPlaces.Rcon (
    RconMode(..),
    ProtocolOptions(..),
    RconInfo(..),
    RconConnection,
    defaultRcon,
    makeRcon,
    connect,
    close,
    isConnected,
    send,
    recv,
    recvRcon,
    enableLog,
    disableLog,
    setPassword,
    setMode,
    setTimeDiff,
    getPassword,
    getMode,
    getTimeDiff,
    getHost,
    getPort
) where
import DarkPlaces.Rcon.Internal
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Network.Socket hiding (connect, close, isConnected, send, recv)
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB
import qualified Network.Socket.ByteString.Lazy as NBL
import Data.IORef
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad
import Control.Applicative


data RconMode = NonSecureRcon
              | TimeSecureRcon
              | ChallangeSecureRcon
    deriving(Show, Read, Eq, Ord, Enum, Bounded)


data ProtocolOptions = OnlyIPv4
                     | OnlyIPv6
                     | BothProtocols
    deriving(Show, Read, Eq, Ord, Enum, Bounded)


data RconInfo = RconInfo {
    rconHost     :: HostName,
    rconPort     :: ServiceName,
    rconMode     :: RconMode,
    rconPassword :: B.ByteString,
    rconTimeDiff :: Int,
    rconProtoOpt :: ProtocolOptions
} deriving (Show, Read, Eq)


data RconConnection = RconConnection {
    connSocket :: Socket,
    connInfo :: IORef RconInfo,
    getChallange :: IO B.ByteString
}


defaultRcon :: RconInfo
defaultRcon = RconInfo {rconHost="localhost",
                        rconPort="26000",
                        rconMode=TimeSecureRcon,
                        rconPassword=B.empty,
                        rconTimeDiff=0,
                        rconProtoOpt=BothProtocols}


sockGetChallenge :: Socket -> IO B.ByteString
sockGetChallenge s = NB.send s challangePacket >> recvChallange
  where
    getResponse = NB.recv s maxPacketSize
    recvChallange = do
        resp <- getResponse
        case parseChallenge resp of
            Just challenge -> return challenge
            Nothing -> recvChallange


createDPSocket :: HostName -> ServiceName -> ProtocolOptions -> IO Socket
createDPSocket host port opt = do
    host_addr <- getHostAddr
    sock <- socket (addrFamily host_addr) Datagram (addrProtocol host_addr)
    N.connect sock (addrAddress host_addr)
    return sock
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
    sock <- createDPSocket host port opt
    info_ref <- newIORef rcon
    return RconConnection {connSocket=sock,
                           connInfo=info_ref,
                           getChallange=sockGetChallenge sock}
  where
    host = rconHost rcon
    port = rconPort rcon
    opt = rconProtoOpt rcon


-- | Close connection
close :: RconConnection -> IO ()
close = N.close . connSocket

-- | Return True if connection is active 
-- and False if it closed
isConnected :: RconConnection -> IO Bool
isConnected = N.isConnected . connSocket

-- | Sends rcon command via `RconConnection`
send :: RconConnection -> B.ByteString -> IO ()
send conn command = void $ NBL.send (connSocket conn) =<< rconPacket
  where
    rconNonsec rcon = rconNonSecurePacket (rconPassword rcon) command
    rconSecTime rcon = do
        cur_time <- getPOSIXTime
        let send_time = realToFrac cur_time + fromIntegral (rconTimeDiff rcon) :: Double
        return $ rconSecureTimePacket send_time (rconPassword rcon) command
    rconSecChallange rcon = do
        challange <- getChallange conn
        return $ rconSecureChallangePacket challange (rconPassword rcon) command
    rconPacket = do
        rcon <- readIORef $ connInfo conn
        case rconMode rcon of
            NonSecureRcon -> return $ rconNonsec rcon
            TimeSecureRcon -> rconSecTime rcon
            ChallangeSecureRcon -> rconSecChallange rcon

-- | Receive packet and tries parse it as rcon packet.
-- If succeeds returns Right ByteString with parsed response 
-- else returns Left ByteString with raw packet
recv :: RconConnection -> IO (Either B.ByteString B.ByteString)
recv conn = do
    resp <- NB.recv sock maxPacketSize
    return $ maybe (Left resp) Right $ parseRcon resp
  where
    sock = connSocket conn

-- | Waits for rcon packet and return parsed response.
-- if parsing fails it discard packet and waits another.
recvRcon :: RconConnection -> IO B.ByteString
recvRcon conn = do
    e_resp <- recv conn
    either (const $ recvRcon conn) return e_resp


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
enableLog :: RconConnection -> IO ()
enableLog c = send c =<< enableLogStr c

-- | Opposite action for `enableLog`
disableLog :: RconConnection -> IO ()
disableLog c = send c =<< disableLogStr c


setParam :: RconConnection -> (RconInfo -> RconInfo) -> IO ()
setParam c f = atomicModifyIORef (connInfo c) $ \r -> (f r, ())


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
