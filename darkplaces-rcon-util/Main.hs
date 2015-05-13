module Main where
import DRcon.CommandArgs
import DRcon.ConfigFile
import DRcon.Paths
import DRcon.EvalParser
import DarkPlaces.Rcon hiding (connect, send, getPassword)
import qualified DarkPlaces.Rcon as RCON
import DarkPlaces.Text
import Options.Applicative
import qualified Data.ByteString.Lazy as BL
import System.Timeout
import qualified Data.ByteString.UTF8 as BU
import Control.Monad.Error
import System.Console.Haskeline
import System.Console.Haskeline.History (historyLines)
import Control.Monad.State.Strict
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding as TE
import Text.Printf
import Data.Char (toUpper)

#ifdef CABAL_VERSION
import Paths_darkplaces_rcon_util (version)
import Data.Version (showVersion)

versionStr = showVersion version
#else
versionStr = "dev"
#endif

versionStr :: String


versionInfo :: String
versionInfo = "drcon " ++ versionStr


data ReplState = ReplState {
    replLastCmd  :: Maybe InputType,
    replColor    :: Bool,
    replTimeout  :: Float,
    replEncoding :: DecodeType
} deriving (Eq, Show)


type Repl m = InputT (StateT ReplState m)


updateLastCmd :: (Monad m) => InputType -> Repl m ()
updateLastCmd cmd = lift $ modify (\s -> s {replLastCmd=Just cmd})


toMicroseconds :: Float -> Int
toMicroseconds v = round $ v * 1e6


printRecv :: RconConnection -> (Float, Bool, DecodeType) -> BinStreamState -> IO ()
printRecv con parm@(time, color, enc) st = do
    mdata <- timeout (toMicroseconds time) $ RCON.recvRcon con
    let st_args = PrintStreamArgs {
        withColor=color,
        streamState=st,
        decodeFun=toUTF enc}

    case mdata of
        (Just r) -> printStreamDPText st_args (BL.fromStrict r) >>= printRecv con parm
        Nothing -> streamEnd color st


rconExec :: DRconArgs -> String -> Bool -> IO ()
rconExec dargs command color = do
    con <- RCON.connect (connectInfo dargs)
    RCON.send con (BU.fromString command)
    printRecv con (time, color, enc) defaultStreamState
    RCON.close con
  where
    time = drconTimeout dargs
    enc = drconEncoding dargs


replLoop :: RconConnection -> Repl IO ()
replLoop con = handle (\Interrupt -> replLoop con) $ withInterrupt $ do
    minput <- getInputLine "> "
    case minput of
        Nothing    -> liftIO $ RCON.close con
        Just input -> case parseCommand input of
            Left e -> do
                outputStrLn $ show e
                replLoop con
            Right a -> replAction con a


replAction :: RconConnection -> InputType -> Repl IO ()
replAction con cmd = case cmd of
    Quit -> liftIO $ RCON.close con
    Empty -> replLoop con
    RepeatLast -> do
        last <- lift $ replLastCmd <$> get
        case last of
            Nothing -> outputStrLn noLastCmd >> replLoop con
            Just last_cmd -> replAction con last_cmd
    Help -> do
        outputStrLn $ T.unpack helpMessage
        updateLastCmd cmd
        replLoop con
    ListVars -> do
        outputStrLn $ T.unpack helpVars
        updateLastCmd cmd
        replLoop con
    Show var -> do
        val <- showVar <$> case var of
            Mode -> liftIO $ SetMode <$> getMode con
            TimeDiff -> liftIO $ SetTimeDiff <$> getTimeDiff con
            Timeout -> lift $ SetTimeout . replTimeout <$> get
            Encoding -> lift $ SetEncoding . replEncoding <$> get
            Color -> lift $ SetColor . replColor <$> get

        outputStrLn $ printf "%s: %s" (toTitle $ show var) val
        updateLastCmd cmd
        replLoop con
    Set var_set -> do
        case var_set of
            SetMode mode -> liftIO $ setMode con mode
            SetTimeDiff diff -> liftIO $ setTimeDiff con diff
            SetTimeout t -> lift $ modify (\s -> s {replTimeout=t})
            SetEncoding enc -> lift $ modify (\s -> s {replEncoding=enc})
            SetColor c -> lift $ modify (\s -> s {replColor=c})

        updateLastCmd cmd
        replLoop con
    Version -> do
        outputStrLn versionInfo
        updateLastCmd cmd
        replLoop con
    Login -> do
        pwd <- getPassword Nothing "Password: "
        case pwd of
            (Just pass) -> liftIO $ RCON.setPassword con (BU.fromString pass)
            Nothing -> return ()
        replLoop con
    History v -> do
        -- 10 is default value for history
        let num = fromMaybe 10 v
        history <- getHistory
        let h_lines = take num $ historyLines history
        forM_ h_lines $ outputStrLn . (replicate 2 ' ' ++)
        updateLastCmd cmd
        replLoop con
    RconCommand command -> do
        rconEval con $ TE.encodeUtf8 command
        updateLastCmd cmd
        replLoop con
  where
    toTitle s = (\(f, e) -> map toUpper f ++ e) $ splitAt 1 s
    rconEval con command = do
        repl_state <- lift get
        let time = replTimeout repl_state
            color = replColor repl_state
            enc = replEncoding repl_state

        liftIO $ RCON.send con command
        liftIO $ printRecv con (time, color, enc) defaultStreamState

    noLastCmd = "there is no last command to perform\n" ++
        "use :? for help."


rconRepl :: DRconArgs -> Bool -> IO ()
rconRepl dargs color = do
    con <- RCON.connect (connectInfo dargs)
    hist_path <- historyPath
    let base_settings = defaultSettings {historyFile=Just hist_path,
                                         autoAddHistory=True}
    let repl_state = ReplState {replLastCmd=Nothing,
                                replColor=color,
                                replTimeout=time,
                                replEncoding=enc}

    let hline_settings = setComplete comp base_settings
    evalStateT (runInputT hline_settings $ replLoop con) repl_state
  where
    time = drconTimeout dargs
    enc = drconEncoding dargs
    compliter prev cmd = return $
        (simpleCompletion . T.unpack) <$> internalAutoComplete (T.pack prev) (T.pack cmd)
    comp = completeWordWithPrev Nothing " \t" compliter


processArgs :: Maybe CommandArgs -> UtilError ()
processArgs Nothing = liftIO $ putStrLn versionInfo
processArgs (Just args) = do
    drcon_args <- rconConfigure (cliServerName args) (cliBaseArgs args)
    color <- liftIO $ case cliColor args of
        (Just c) -> return c
        Nothing -> supportColors

    case cliCommand args of
        (Just command) -> liftIO $ rconExec drcon_args command color
        Nothing -> liftIO $ rconRepl drcon_args color


main :: IO ()
main = handleErrors . processArgs =<< execParser opts
  where
    opts = info (helper <*> maybeArgsParser)
        (fullDesc <> progDesc "Darkplaces rcon client utility")
    handleErrors me = do
        r <- runErrorT me
        case r of
            (Right ()) -> return ()
            (Left e) -> putStrLn e
