module DRcon.CommandArgs (
    ProtocolOptions(..),
    BaseArgs(..),
    CommandArgs(..),
    parseRconMode,
    checkTimeout,
    parseEncoding,
    parseColorMode,
    argsParser,
    maybeArgsParser
) where
import Options.Applicative
import DarkPlaces.Text (DecodeType(..))
import DarkPlaces.Rcon hiding (connect, send)
import DRcon.Polyfills (readMaybe)


data BaseArgs = BaseArgs {
    confServerString :: String,
    confPassword     :: Maybe String,
    confMode         :: Maybe RconMode,
    confTimeDiff     :: Maybe Int,
    confTimeout      :: Maybe Float,
    confEncoding     :: Maybe DecodeType,
    confProtoOptions :: Maybe ProtocolOptions
} deriving(Show, Read, Eq)


data CommandArgs = CommandArgs {
    cliServerName   :: String,
    cliBaseArgs     :: BaseArgs,
    cliColor        :: Maybe Bool,
    cliCommand      :: Maybe String
} deriving(Show, Read, Eq)


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


parseColorMode :: String -> ReadM (Maybe Bool)
parseColorMode mode_str
    | mode_str == "always" = return $ Just True
    | mode_str == "auto" = return Nothing
    | mode_str == "never" = return $ Just False
    | otherwise = readerError "value should be always, auto or never"


protoOptionsParser :: Parser (Maybe ProtocolOptions)
protoOptionsParser =
    flag' (Just OnlyIPv4) (short '4' <> help "Forces to use IPv4 addresses only")
    <|> flag' (Just OnlyIPv6) (short '6' <> help "Forces to use IPv6 addresses only")
    <|> (pure Nothing)


commandParser :: Parser String
commandParser = unwords <$> (some $ argument str (
    metavar "COMMAND"
    <> help "Command that will be send to server"))


argsParser :: Parser CommandArgs
argsParser = argsConstructor
    <$> protoOptionsParser
    <*> optional (strOption (
        short 'p'
        <> long "password"
        <> help "Server's password"
        <> metavar "PASSWORD"))
    <*> optional (option (str >>= eitherArgs . parseRconMode) (
        short 'm'
        <> long "mode"
        <> help "Use secure rcon, same as `rcon_secure' cvar, 1 is default"
        <> metavar "MODE"))
    <*> optional (option auto (
        short 'd'
        <> long "time-diff"
        <> help "Integer difference between client and server time, can be negative"
        <> metavar "TIMEDIFF"))
    <*> optional (option (auto >>= eitherArgs . checkTimeout) (
        short 't'
        <> long "timeout"
        <> help "How many time wait for reponse after send or previous response"
        <> metavar "TIMEOUT"))
    <*> optional (option (str >>= eitherArgs . parseEncoding) (
        short 'e'
        <> long "encoding"
        <> help "Server encoding. Major options is `utf8' and `nexuiz'"
        <> metavar "ENCODING"))
    <*> option (str >>= parseColorMode) (
        long "color"
        <> value Nothing
        <> help "Possible values are: `auto', `always' and `never'"
        <> metavar "COLOR_MODE")
    <*> argument str (
        help "Server to connect or config section"
        <> metavar "SERVER")
    <*> optional commandParser
  where
    argsConstructor protoOpt password mode tdiff tout enc color server cmd =
        let baseArgs = BaseArgs {confServerString=server,
                                 confPassword=password,
                                 confMode=mode,
                                 confTimeDiff=tdiff,
                                 confTimeout=tout,
                                 confEncoding=enc,
                                 confProtoOptions=protoOpt}
        in CommandArgs {cliServerName=server,
                        cliBaseArgs=baseArgs,
                        cliColor=color,
                        cliCommand=cmd}


maybeArgsParser :: Parser (Maybe CommandArgs)
maybeArgsParser = flag' Nothing (
    long "version"
    <> short 'V'
    <> hidden) <|> (Just <$> argsParser)
