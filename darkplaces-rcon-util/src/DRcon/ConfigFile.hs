{-# LANGUAGE FlexibleContexts #-}
module DRcon.ConfigFile (
    UtilError,
    readConfig,
    getArgsFromConfig,
    rconConfigure
) where
import DRcon.CommandArgs
import DRcon.Paths
import DarkPlaces.Text (DecodeType(..))
import Data.ConfigFile
import Text.Printf
import System.IO.Error (isDoesNotExistError)
import Control.Exception (tryJust)
import qualified Data.ByteString.UTF8 as BU
import Network.HostAndPort (defaultHostAndPort)
import DarkPlaces.Rcon hiding (connect, send)
import Control.Monad.Error
import Control.Applicative
import Data.Monoid
import Data.Maybe


type UtilError = ErrorT String IO


getMaybe :: (Get_C a, MonadError CPError m) => ConfigParser -> SectionSpec -> OptionSpec -> m (Maybe a)
getMaybe c sec opt = (Just `liftM` get c sec opt) `catchError` const (return Nothing)


readConfig :: String -> UtilError (Maybe ConfigParser)
readConfig cpath = do
    r <- liftIO $ tryJust (guard . isDoesNotExistError) doRead

    case either (const Nothing) Just r of
        (Just (Right v)) -> return $ Just v
        (Just (Left _)) -> throwError "Error while parsing config file"
        Nothing -> return Nothing

  where
    doRead = liftIO $ readfile emptyCP cpath


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
