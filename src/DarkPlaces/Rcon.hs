module DarkPlaces.Rcon where
import Crypto.Hash
import Data.Byteable
import Data.Monoid
import Text.Printf
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Network.Socket.ByteString.Lazy as NBL


maxPacketSize :: Int
maxPacketSize = 1399


quakePacketHeader :: B.ByteString
quakePacketHeader = B.pack [0xFF, 0xFF, 0xFF, 0xFF]


challangePacket :: B.ByteString
challangePacket = B.append quakePacketHeader $ BC.pack "getchallenge"


hmacMD4 :: B.ByteString -> B.ByteString -> B.ByteString
hmacMD4 secret msg = toBytes (hmac secret msg :: HMAC MD4)


parseChallenge :: B.ByteString -> Maybe B.ByteString
parseChallenge packet = if header `B.isPrefixOf` packet
    then Just challenge
    else Nothing
  where
    header = B.append quakePacketHeader $ BC.pack "challenge "
    headerLen = B.length header
    challenge = B.take 11 $ B.drop headerLen packet


rconNonSecurePacket :: B.ByteString -> B.ByteString -> BL.ByteString
rconNonSecurePacket passw command = BB.toLazyByteString builder
  where
    builder = mconcat [BB.byteString quakePacketHeader,
                       BB.string7 "rcon ",
                       BB.byteString passw,
                       BB.char7 ' ',
                       BB.byteString command]


rconSecureTimePacket :: (Fractional a, PrintfArg a) => a -> B.ByteString -> B.ByteString -> BL.ByteString
rconSecureTimePacket t passw command = BB.toLazyByteString builder
  where
    time_str = BB.string7 $ printf "%.6f" t
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
sockGetChallenge s = send s challangePacket >> recvChallange
  where
    getResponse = recv s maxPacketSize
    recvChallange = do
        resp <- getResponse
        case parseChallenge resp of
            Just challenge -> return challenge
            Nothing -> recvChallange
