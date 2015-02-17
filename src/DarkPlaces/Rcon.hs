module DarkPlaces.Rcon where
import Crypto.Hash
import Data.Byteable
import Data.Monoid
import Text.Printf
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL


quakePacketHeader :: B.ByteString
quakePacketHeader = B.pack [0xFF, 0xFF, 0xFF, 0xFF]


challangePacket :: B.ByteString
challangePacket = B.append quakePacketHeader $ BC.pack "getchallenge"


hmacMD4 :: B.ByteString -> B.ByteString -> B.ByteString
hmacMD4 secret msg = toBytes (hmac secret msg :: HMAC MD4)


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
