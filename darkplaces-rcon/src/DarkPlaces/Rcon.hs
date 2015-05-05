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
    recvRcon,
    enableLog,
    disableLog,
    setPassword,
    setMode,
    setTimeDiff
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


getConnectionInfo :: (RconInfo -> a) -> RconConnection -> IO a
getConnectionInfo f = fmap f . readIORef . connInfo


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
    getHostAddr = fmap head $ getAddrInfo (Just hints) (Just host) (Just port)
    dg_hints = defaultHints {addrSocketType=Datagram, addrFlags=[AI_ADDRCONFIG]}
    hints = case opt of
        BothProtocols -> dg_hints
        OnlyIPv4 -> dg_hints {addrFamily=AF_INET}
        OnlyIPv6 -> dg_hints {addrFamily=AF_INET6}


makeRcon :: HostName -> ServiceName -> B.ByteString -> RconInfo
makeRcon host port passw = defaultRcon {rconHost=host,
                                        rconPort=port,
                                        rconPassword=passw}


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


close :: RconConnection -> IO ()
close = N.close . connSocket


isConnected :: RconConnection -> IO Bool
isConnected = N.isConnected . connSocket


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


recvRcon :: RconConnection -> IO B.ByteString
recvRcon conn = do
    resp <- NB.recv sock maxPacketSize
    case parseRcon resp of
        (Just val) -> return val
        Nothing -> recvRcon conn
  where
    sock = connSocket conn


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


enableLog :: RconConnection -> IO ()
enableLog c = send c =<< enableLogStr c


disableLog :: RconConnection -> IO ()
disableLog c = send c =<< disableLogStr c


setParam :: RconConnection -> (RconInfo -> RconInfo) -> IO ()
setParam c f = atomicModifyIORef (connInfo c) $ \r -> (f r, ())


setPassword :: RconConnection -> B.ByteString -> IO ()
setPassword c passw = setParam c $ \rcon -> rcon {rconPassword=passw}


setMode :: RconConnection -> RconMode -> IO ()
setMode c mode = setParam c $ \rcon -> rcon {rconMode=mode}


setTimeDiff :: RconConnection -> Int -> IO ()
setTimeDiff c time = setParam c $ \rcon -> rcon {rconTimeDiff=time}
