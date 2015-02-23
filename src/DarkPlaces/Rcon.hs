module DarkPlaces.Rcon (
    RconMode(..),
    RconInfo(..),
    RconConnection,
    defaultRcon,
    maxPacketSize,
    parseChallenge,
    parseRcon,
    rconNonSecurePacket,
    rconSecureTimePacket,
    rconSecureChallangePacket,
    makeRcon,
    connect,
    close,
    isConnected,
    send,
    recvRcon
) where
import Crypto.Hash
import Data.Byteable
import Data.Monoid
import Text.Printf
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Network.Socket hiding (connect, close, isConnected, send, recv)
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB
import qualified Network.Socket.ByteString.Lazy as NBL
import Data.IORef
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad


data RconMode = NonSecureRcon
              | TimeSecureRcon
              | ChallangeSecureRcon
    deriving(Show, Read, Eq, Ord, Enum, Bounded)


data RconInfo = RconInfo {
    rconHost :: HostName,
    rconPort :: ServiceName,
    rconMode :: RconMode,
    rconPassword :: B.ByteString,
    rconTimeDiff :: Int
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
                        rconTimeDiff=0}


maxPacketSize :: Int
maxPacketSize = 1399


quakePacketHeader :: B.ByteString
quakePacketHeader = B.pack [0xFF, 0xFF, 0xFF, 0xFF]


challangePacket :: B.ByteString
challangePacket = B.append quakePacketHeader $ BC.pack "getchallenge"


hmacMD4 :: B.ByteString -> B.ByteString -> B.ByteString
hmacMD4 secret msg = toBytes (hmac secret msg :: HMAC MD4)


getConnectionInfo :: (RconInfo -> a) -> RconConnection -> IO a
getConnectionInfo f = fmap f . readIORef . connInfo


parseChallenge :: B.ByteString -> Maybe B.ByteString
parseChallenge packet = if header `B.isPrefixOf` packet
    then Just challenge
    else Nothing
  where
    header = B.append quakePacketHeader $ BC.pack "challenge "
    headerLen = B.length header
    challenge = B.take 11 $ B.drop headerLen packet


parseRcon :: B.ByteString -> Maybe B.ByteString
parseRcon packet = if header `B.isPrefixOf` packet
    then Just response
    else Nothing
  where
    header = B.append quakePacketHeader $ BC.pack "n"
    headerLen = B.length header
    response = B.drop headerLen packet


rconNonSecurePacket :: B.ByteString -> B.ByteString -> BL.ByteString
rconNonSecurePacket passw command = BB.toLazyByteString builder
  where
    builder = mconcat [BB.byteString quakePacketHeader,
                       BB.string7 "rcon ",
                       BB.byteString passw,
                       BB.char7 ' ',
                       BB.byteString command]


rconSecureTimePacket :: (Real a) => a -> B.ByteString -> B.ByteString -> BL.ByteString
rconSecureTimePacket t passw command = BB.toLazyByteString builder
  where
    time_str = BB.string7 $ printf "%.6f" $ (realToFrac t :: Double)
    time_command = mconcat [time_str, BB.char7 ' ', BB.byteString command]
    time_command_str = BL.toStrict $ BB.toLazyByteString time_command
    key = BB.byteString $ hmacMD4 passw time_command_str
    builder = mconcat [BB.byteString quakePacketHeader,
                       BB.string7 "srcon HMAC-MD4 TIME ",
                       key, BB.char7 ' ',
                       BB.byteString time_command_str]


rconSecureChallangePacket :: B.ByteString -> B.ByteString -> B.ByteString -> BL.ByteString
rconSecureChallangePacket challenge passw command = BB.toLazyByteString builder
  where
    hmac_key = BL.toStrict $ BL.fromChunks [challenge, BC.pack " ", command]
    key = BB.byteString $ hmacMD4 passw hmac_key
    builder = mconcat [BB.byteString quakePacketHeader,
                       BB.string7 "srcon HMAC-MD4 CHALLENGE ",
                       key, BB.char7 ' ', BB.byteString challenge,
                       BB.char7 ' ', BB.byteString command]


sockGetChallenge :: Socket -> IO B.ByteString
sockGetChallenge s = NB.send s challangePacket >> recvChallange
  where
    getResponse = NB.recv s maxPacketSize
    recvChallange = do
        resp <- getResponse
        case parseChallenge resp of
            Just challenge -> return challenge
            Nothing -> recvChallange


createDPSocket :: HostName -> ServiceName -> IO Socket
createDPSocket host port = do
    host_addr <- getHostAddr
    sock <- socket (addrFamily host_addr) Datagram defaultProtocol
    N.connect sock (addrAddress host_addr)
    return sock
  where
    getHostAddr = fmap head $ getAddrInfo Nothing (Just host) (Just port)


makeRcon :: HostName -> ServiceName -> B.ByteString -> RconInfo
makeRcon host port passw = defaultRcon {rconHost=host,
                                        rconPort=port,
                                        rconPassword=passw}


connect :: RconInfo -> IO RconConnection
connect rcon = do
    sock <- createDPSocket host port
    info_ref <- newIORef rcon
    return RconConnection {connSocket=sock,
                           connInfo=info_ref,
                           getChallange=sockGetChallenge sock}
  where
    host = rconHost rcon
    port = rconPort rcon


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
