module DRcon.CommandArgs (
    ConnectionArgs(..),
    CommandArgs(..),
    defaultConnectionArgs,
    mergeConnectionArgs,
    parseRconMode,
    checkTimeout,
    parseEncoding,
    connectionArgsParser,
    parseColorMode,
    argsParser,
    maybeArgsParser
) where
import Options.Applicative
import Data.List (lookup)
import Data.Monoid
import DarkPlaces.Text (DecodeType(..))
import DarkPlaces.Rcon hiding (connect, send)
import DRcon.Polyfills (readMaybe)


data ConnectionArgs = ConnectionArgs {
    conServerString :: Maybe String,
    conPassword  :: Maybe String,
    conMode      :: Maybe RconMode,
    conTimeDiff  :: Maybe Int,
    conTimeout   :: Maybe Float,
    conEncoding  :: Maybe DecodeType
} deriving(Show, Read, Eq)


data CommandArgs = CommandArgs {
    conArgs      :: ConnectionArgs,
    cliColor     :: Maybe Bool,
    cliCommand   :: Maybe String
} deriving(Show, Read, Eq)


defaultConnectionArgs = ConnectionArgs {
    conServerString=Nothing,
    conPassword=Nothing,
    conMode=Just TimeSecureRcon,
    conTimeDiff=Just 0,
    conTimeout=Just 1.5,
    conEncoding=Just Utf8Lenient}


maybeP :: ReadM a -> ReadM (Maybe a)
maybeP = fmap Just


mergeConnectionArgs :: ConnectionArgs -> ConnectionArgs -> ConnectionArgs
mergeConnectionArgs f l = l {
    conServerString = getLast $ Last (conServerString f) <> Last (conServerString l),
    conPassword = getLast $ Last (conPassword f) <> Last (conPassword l),
    conMode = getLast $ Last (conMode f) <> Last (conMode l),
    conTimeDiff = getLast $ Last (conTimeDiff f) <> Last (conTimeDiff l),
    conTimeout = getLast $ Last (conTimeout f) <> Last (conTimeout l),
    conEncoding = getLast $ Last (conEncoding f) <> Last (conEncoding l)}


instance Monoid ConnectionArgs where
    mempty = ConnectionArgs Nothing Nothing Nothing Nothing Nothing Nothing
    mappend = mergeConnectionArgs


eitherArgs :: Either String a -> ReadM a
eitherArgs (Right v) = return v
eitherArgs (Left msg) = readerError msg


parseRconMode :: String -> Either String RconMode
parseRconMode ms = case readMaybe ms of
    Just x | x >= 0 && x < 3 -> Right $ toEnum x
    _ -> Left "value should be 0, 1 or 2"


checkTimeout :: Float -> Either String Float
checkTimeout t
    | t > 0 = Right t
    | otherwise = Left "value should be bigger then 0"


encodings :: [(String, DecodeType)]
encodings = [
    ("utf8", Utf8Lenient),
    ("utf8-lenient", Utf8Lenient),
    ("utf8-ignore", Utf8Ignore),
    ("utf8-strict", Utf8Strict),
    ("nexuiz", NexuizDecode)]


parseEncoding :: String -> Either String DecodeType
parseEncoding encname = case lookup encname encodings of
    Just v -> Right v
    Nothing -> Left "Error bad decode type"


connectionArgsParser :: Parser ConnectionArgs
connectionArgsParser = ConnectionArgs
    <$> option (maybeP str) (
        short 's'
        <> long "server"
        <> value Nothing
        <> help "Server to connect or config section"
        <> metavar "SERVER")
    <*> option (maybeP str) (
        short 'p'
        <> value Nothing
        <> long "password"
        <> help "Server's password"
        <> metavar "PASSWORD")
    <*> option (maybeP $ str >>= eitherArgs . parseRconMode) (
        short 'm'
        <> long "mode"
        <> value Nothing
        <> help "Use secure rcon, same as `rcon_secure' cvar, 1 is default"
        <> metavar "MODE")
    <*> option (maybeP auto) (
        short 'd'
        <> long "time-diff"
        <> value Nothing
        <> help "Integer difference between client and server time, can be negative"
        <> metavar "TIMEDIFF")
    <*> option (maybeP $ auto >>= eitherArgs . checkTimeout) (
        short 't'
        <> long "timeout"
        <> value Nothing
        <> help "How many time wait for reponse after send or previous response"
        <> metavar "TIMEOUT")
    <*> option (maybeP $ str >>= eitherArgs . parseEncoding) (
        short 'e'
        <> long "encoding"
        <> value Nothing
        <> help "Server encoding. Major options is `utf8' and `nexuiz'"
        <> metavar "ENCODING")


parseColorMode :: String -> ReadM (Maybe Bool)
parseColorMode mode_str
    | mode_str == "always" = return $ Just True
    | mode_str == "auto" = return Nothing
    | mode_str == "never" = return $ Just False
    | otherwise = readerError "value should be always, auto or never"


commandParser :: Parser String
commandParser = unwords <$> (some $ argument str (
    metavar "COMMAND"
    <> help "Command that will be send to server"))


argsParser :: Parser CommandArgs
argsParser = CommandArgs
    <$> connectionArgsParser
    <*> (option $ str >>= parseColorMode) (
        long "color"
        <> value Nothing
        <> help "Possible values are: `auto', `always' and `never'"
        <> metavar "COLOR_MODE")
    <*> optional commandParser


maybeArgsParser :: Parser (Maybe CommandArgs)
maybeArgsParser = flag' Nothing (
    long "version"
    <> short 'V'
    <> hidden) <|> (Just <$> argsParser)
