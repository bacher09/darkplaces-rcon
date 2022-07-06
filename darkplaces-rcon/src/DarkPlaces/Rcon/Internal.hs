module DarkPlaces.Rcon.Internal where
import Crypto.Hash
import Data.Byteable
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Text.Printf

-- | max Darkplaces packet size
maxPacketSize :: Int
maxPacketSize = 1400


quakePacketHeader :: B.ByteString
quakePacketHeader = B.pack [0xFF, 0xFF, 0xFF, 0xFF]


challengePacket :: B.ByteString
challengePacket = B.append quakePacketHeader $ BC.pack "getchallenge"


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

-- | Try to parse packet as dp rcon packet.
-- if parsing fails returns Nothing
parseRcon :: B.ByteString -> Maybe B.ByteString
parseRcon packet = if header `B.isPrefixOf` packet
    then Just response
    else Nothing
  where
    header = B.append quakePacketHeader $ BC.pack "n"
    headerLen = B.length header
    response = B.drop headerLen packet


-- | generates simple quake rcon packet
rconNonSecurePacket :: B.ByteString
                    -- ^ password
                    -> B.ByteString
                    -- ^ command
                    -> BL.ByteString
rconNonSecurePacket passw command = BB.toLazyByteString builder
  where
    builder = mconcat [BB.byteString quakePacketHeader,
                       BB.string7 "rcon ",
                       BB.byteString passw,
                       BB.char7 ' ',
                       BB.byteString command]


-- | generates time based secure packet
rconSecureTimePacket :: (Real a) => a
                                 -- ^ Current unix timestamp
                                 -> B.ByteString
                                 -- ^ password
                                 -> B.ByteString
                                 -- ^ comand
                                 -> BL.ByteString
rconSecureTimePacket t passw command = BB.toLazyByteString builder
  where
    time_str = BB.string7 $ printf "%.6f" (realToFrac t :: Double)
    time_command = mconcat [time_str, BB.char7 ' ', BB.byteString command]
    time_command_str = BL.toStrict $ BB.toLazyByteString time_command
    key = BB.byteString $ hmacMD4 passw time_command_str
    builder = mconcat [BB.byteString quakePacketHeader,
                       BB.string7 "srcon HMAC-MD4 TIME ",
                       key, BB.char7 ' ',
                       BB.byteString time_command_str]

-- | generates chalange based secure packet
rconSecureChallengePacket :: B.ByteString
                          -- ^ connection's challenge
                          -> B.ByteString
                          -- ^ password
                          -> B.ByteString
                          -- ^ command
                          -> BL.ByteString
rconSecureChallengePacket challenge passw command = BB.toLazyByteString builder
  where
    hmac_key = BL.toStrict $ BL.fromChunks [challenge, BC.pack " ", command]
    key = BB.byteString $ hmacMD4 passw hmac_key
    builder = mconcat [BB.byteString quakePacketHeader,
                       BB.string7 "srcon HMAC-MD4 CHALLENGE ",
                       key, BB.char7 ' ', BB.byteString challenge,
                       BB.char7 ' ', BB.byteString command]
