{-# LANGUAGE FlexibleContexts #-}
module DRcon.ConfigFile (
    DRconArgs(..),
    UtilError,
    rconConfigure
) where
import DRcon.CommandArgs
import DRcon.Paths
import DRcon.Prompt (defaultPrompt, promptEnvName, readPrompt)
import DRcon.Polyfills (lookupEnv)
import DarkPlaces.Text (DecodeType(..))
import Data.ConfigFile
import System.IO.Error (isDoesNotExistError)
import Control.Exception (tryJust)
import Control.Monad.Catch (handle)
import qualified Data.ByteString.UTF8 as BU
import Network.HostAndPort (defaultHostAndPort)
import DarkPlaces.Rcon hiding (connect, send, getPassword)
import System.Console.Haskeline
import Control.Monad.Except
import Control.Applicative
import Data.Maybe
import System.Exit


data DRconArgs = DRconArgs {
    connectInfo   :: RconInfo,
    connectName   :: String,
    drconTimeout  :: Float,
    drconEncoding :: DecodeType,
    drconPrompt   :: String
} deriving (Show, Read, Eq)


type UtilError = ExceptT String IO


getMaybe :: (Get_C a, MonadError CPError m) => ConfigParser -> SectionSpec -> OptionSpec -> m (Maybe a)
getMaybe c sec opt = (Just <$> get c sec opt) `catchError` const (return Nothing)


readConfig :: String -> UtilError (Maybe ConfigParser)
readConfig cpath = do
    r <- liftIO $ tryJust (guard . isDoesNotExistError) doRead

    case either (const Nothing) Just r of
        (Just (Right v)) -> return $ Just v
        (Just (Left _)) -> throwError "Error while parsing config file"
        Nothing -> return Nothing

  where
    doRead = liftIO $ readfile emptyCP cpath


parseAddrFamily :: String -> Either String ProtocolOptions
parseAddrFamily val = case val of
    "any"   -> Right BothProtocols
    "inet"  -> Right OnlyIPv4
    "inet6" -> Right OnlyIPv6
    _       -> Left "Bad addrfamily, supported options: any, inet, inet6"


getPrompt :: Maybe ConfigParser -> UtilError String
getPrompt mconf = do
    conf_prompt <- case mconf of
        Just c -> case getMaybe c "DEFAULT" "prompt" of
            Right v -> return v
            Left _ -> throwError "Error while parsing file"
        Nothing -> return Nothing

    env_prompt <- liftIO $ lookupEnv promptEnvName
    let prompt = fromMaybe defaultPrompt (conf_prompt <|> env_prompt)

    case readPrompt prompt of
        (Right v) -> return v
        (Left s)  -> throwError s


argsFromConfig :: (MonadError CPError m) => ConfigParser -> String -> m BaseArgs
argsFromConfig c name = do
    server <- if c `has_section` name
        then fromMaybe name <$> getMaybe c name "server"
        else return name

    password <- getMaybe c name "password"
    raw_mode <- getMaybe c name "mode"
    mode <- case parseRconMode <$> raw_mode of
        (Just (Right v)) -> return $ Just v
        (Just (Left e)) -> throwError (ParseError $ "Bad value for mode:" ++ e, "getmode")
        Nothing -> return Nothing

    diff <- getMaybe c name "diff"
    raw_timeout <- getMaybe c name "timeout"
    timeout <- case checkTimeout <$> raw_timeout of
        (Just (Right t)) -> return $ Just t
        (Just (Left e)) -> throwError (ParseError $ "Bad timeout:" ++ e, "gettimeout")
        Nothing -> return Nothing

    raw_enc <- getMaybe c name "encoding"
    enc <- case parseEncoding <$> raw_enc of
        (Just (Right t)) -> return $ Just t
        (Just (Left e)) -> throwError (ParseError $ "Bad encoding:" ++ e, "getencoding")
        Nothing -> return Nothing

    raw_protoopts <- getMaybe c name "addrfamily"
    proto_opts <- case parseAddrFamily <$> raw_protoopts of
        (Just (Right t)) -> return $ Just t
        (Just (Left e)) -> throwError (ParseError $ "Bad addrfamily:" ++ e, "getaddrfamily")
        Nothing -> return Nothing

    return $ BaseArgs {
        confServerString=server,
        confPassword=password,
        confMode=mode,
        confTimeDiff=diff,
        confTimeout=timeout,
        confEncoding=enc,
        confProtoOptions=proto_opts}


getPasswordOrExit :: IO String
getPasswordOrExit = do
    mpassw <- runInputT defaultSettings tryGetPassword
    case mpassw of
        (Just p) -> return p
        Nothing -> exitSuccess
  where
    tryGetPassword = handle (\Interrupt -> liftIO exitSuccess) $ withInterrupt $
        getPassword Nothing "Password: "


getDRconArgs :: String -> BaseArgs -> String -> UtilError DRconArgs
getDRconArgs name args prompt = do
    (host, port) <- case defaultHostAndPort "26000" server of
        Nothing -> throwError "Error while parsing server string"
        (Just v) -> return v

    password <- case confPassword args of
        Nothing -> liftIO getPasswordOrExit
        (Just v) -> return v

    let base_rcon = makeRcon host port (BU.fromString password)
    let rcon = base_rcon {rconMode=mode,
                          rconTimeDiff=time_diff,
                          rconProtoOpt=proto_opt}
    return $ DRconArgs {connectInfo=rcon,
                        drconTimeout=time_out,
                        drconEncoding=enc,
                        connectName=name,
                        drconPrompt=prompt}
  where
    server = confServerString args
    mode = fromMaybe TimeSecureRcon $ confMode args
    time_diff = fromMaybe 0 $ confTimeDiff args
    time_out = fromMaybe 1.5 $ confTimeout args
    enc = fromMaybe Utf8Lenient $ confEncoding args
    proto_opt = fromMaybe BothProtocols $ confProtoOptions args


mergeArgs :: BaseArgs -> BaseArgs -> BaseArgs
mergeArgs f s = BaseArgs {
    confServerString = confServerString s,
    confPassword = merge confPassword,
    confMode = merge confMode,
    confTimeDiff = merge confTimeDiff,
    confTimeout = merge confTimeout,
    confEncoding = merge confEncoding,
    confProtoOptions = merge confProtoOptions}
  where
    merge fun = fun f <|> fun s


rconConfigure :: String -> BaseArgs -> UtilError DRconArgs
rconConfigure name args = do
    config_file <- liftIO configPath
    mconf <- readConfig config_file
    new_args <- case argsFromConfig <$> mconf <*> pure name of
        Nothing -> return args
        (Just (Right c)) -> return $ mergeArgs args c
        (Just (Left _)) -> throwError "Error while parsing config"

    prompt <- getPrompt mconf
    getDRconArgs name new_args prompt
