module DarkPlaces.Rcon where
import Crypto.Hash
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL


quakePacketHeader :: B.ByteString
quakePacketHeader = B.pack [0xFF, 0xFF, 0xFF, 0xFF]


rconNonSecurePacket :: B.ByteString -> B.ByteString -> BL.ByteString
rconNonSecurePacket passw command = BB.toLazyByteString builder
  where
    builder = mconcat [BB.byteString quakePacketHeader,
                       BB.string7 "rcon ",
                       BB.byteString passw,
                       BB.char7 ' ',
                       BB.byteString command]
