{-# LANGUAGE FlexibleContexts #-}
module DRcon.Util (
    ConnectionArgs(..),
    UtilError,
    defaultConnectionArgs,
    mergeConnectionArgs,
    connParser,
    configName,
    configPath,
    readConfig,
    rconConfigure,
    historyPath
) where
import DarkPlaces.Rcon hiding (connect, send)
import Options.Applicative
import Data.Monoid
import Data.ConfigFile
import Control.Monad.Error
import Data.Either
import Data.Maybe
import System.Directory (getHomeDirectory, getAppUserDataDirectory)
import System.FilePath
import System.Exit
import System.IO.Error (isDoesNotExistError)
import Control.Exception (tryJust)
import Network.HostAndPort (defaultHostAndPort)
import qualified Data.ByteString.UTF8 as BU
import Text.Printf
import DarkPlaces.Text (DecodeType(..))
import Data.List (lookup)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catchJust)
import Control.Monad (guard)


#if MIN_VERSION_base(4,6,0)
import Text.Read (readMaybe)
import System.Environment (lookupEnv)
#else
import System.Environment (getEnv)

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing


lookupEnv :: String -> IO (Maybe String)
lookupEnv name = catchJust (guard . isDoesNotExistError) (Just <$> getEnv name) (\e -> return Nothing)

#endif


data ConnectionArgs = ConnectionArgs {
    conServerString :: Maybe String,
    conPassword  :: Maybe String,
    conMode      :: Maybe RconMode,
    conTimeDiff  :: Maybe Int,
    conTimeout   :: Maybe Float,
    conEncoding  :: Maybe DecodeType
} deriving(Show, Read, Eq)


type UtilError = ErrorT String IO


defaultConnectionArgs = ConnectionArgs {
    conServerString=Nothing,
    conPassword=Nothing,
    conMode=Just TimeSecureRcon,
    conTimeDiff=Just 0,
    conTimeout=Just 1.5,
    conEncoding=Just Utf8Lenient}


configName :: String
configName = "drcon.ini"


configDirPath :: IO String
configDirPath = do
    h_path <- lookupEnv "DRCON_HOME"
    case h_path of
        (Just path) -> return path
        Nothing -> getAppUserDataDirectory "drcon"


configPath :: IO String
configPath = do
    path <- configDirPath
    return $ path </> configName


historyPath :: IO String
historyPath = do
    m_path <- lookupEnv "DRCON_HISTFILE"
    config_dir <- configDirPath
    case m_path of
        (Just path) -> return path
        Nothing -> return $ config_dir </> "drcon_history"


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


parseRconMode :: String -> Either String RconMode
parseRconMode ms = case readMaybe ms of
    Just x | x >= 0 && x < 3 -> Right $ toEnum x
    _ -> Left "value should be 0, 1 or 2"


maybeP :: ReadM a -> ReadM (Maybe a)
maybeP = fmap Just


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



eitherArgs :: Either String a -> ReadM a
eitherArgs (Right v) = return v
eitherArgs (Left msg) = readerError msg


connParser :: Parser ConnectionArgs
connParser = ConnectionArgs
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


getMaybe :: (Get_C a, MonadError CPError m) => ConfigParser -> SectionSpec -> OptionSpec -> m (Maybe a)
getMaybe c sec opt = (Just `liftM` get c sec opt) `catchError` const (return Nothing)


getArgsFromConfig :: (MonadError CPError m) => ConfigParser -> String -> m ConnectionArgs
getArgsFromConfig c name = do
    server <- getMaybe c name "server"
    password <- getMaybe c name "password"
    raw_mode <- getMaybe c name "mode"
    mode <- case parseRconMode <$> raw_mode of
        (Just (Right v)) -> return $ Just v
        (Just (Left e)) -> throwError (ParseError $ "Bad value for mode:" ++ e, "getmode")
        Nothing -> return $ Nothing
    diff <- getMaybe c name "diff"
    raw_timeout <- getMaybe c name "timeout"
    timeout <- case checkTimeout <$> raw_timeout of
        (Just (Right t)) -> return $ Just t
        (Just (Left e)) -> throwError (ParseError $ "Bad timeout:" ++ e, "gettimeout")
        Nothing -> return $ Nothing

    raw_enc <- getMaybe c name "encoding"
    enc <- case parseEncoding <$> raw_enc of
        (Just (Right t)) -> return $ Just t
        (Just (Left e)) -> throwError (ParseError $ "Bad encoding:" ++ e, "getencoding")
        Nothing -> return $ Nothing

    return $ ConnectionArgs {
        conServerString=server,
        conPassword=password,
        conMode=mode,
        conTimeDiff=diff,
        conTimeout=timeout,
        conEncoding=enc}


toRconInfo :: ConnectionArgs -> Either String RconInfo
toRconInfo args = do
    server <- toEither "server" $ conServerString args
    (host, port) <- toEither "server" $ hostPort server
    password <- toEither "password" $ conPassword args
    mode <- toEither "mode" $ conMode args
    time_diff <- toEither "diff" $ conTimeDiff args
    let rcon = makeRcon host port (BU.fromString password)
    return $ rcon {rconMode=mode, rconTimeDiff=time_diff}
  where
    toEither n Nothing = Left n
    toEither _ (Just v) = Right v
    hostPort = defaultHostAndPort "26000"


readConfig :: String -> UtilError (Maybe ConfigParser)
readConfig cpath = do
    r <- liftIO $ tryJust (guard . isDoesNotExistError) doRead

    case either (const Nothing) Just r of
        (Just (Right v)) -> return $ Just v
        (Just (Left _)) -> throwError "Error while parsing config file"
        Nothing -> return Nothing

  where
    doRead = liftIO $ readfile emptyCP cpath


rconConfigure :: ConnectionArgs -> UtilError (RconInfo, Float, DecodeType)
rconConfigure args = do
    config_file <- liftIO configPath
    mconf <- readConfig config_file
    conf <- case mconf >>= getArgs of
        Nothing -> return $ setDefaults args
        (Just (Right c)) -> return $ setDefaults $ merge args c
        (Just (Left _)) -> throwError "Error while parsing config"

    case toRconInfo conf of
        (Right rcon) -> return (rcon, getTimeout conf, getEncoding conf)
        (Left efield) -> throwError $ printf "Field error \"%s\"" efield
        
  where
    merge f s = (s <> f) {conServerString=conServerString s}
    setDefaults = mergeConnectionArgs defaultConnectionArgs
    server_str = fromMaybe "DEFAULT" $ conServerString args
    getTimeout conf = fromMaybe 1.5 $ conTimeout conf
    getEncoding conf = fromMaybe Utf8Lenient $ conEncoding conf
    getArgs c = if c `has_section` server_str
        then Just $ getArgsFromConfig c server_str
        else Nothing
