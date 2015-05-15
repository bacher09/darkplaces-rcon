module Main where
import DRcon.CommandArgs
import DRcon.ConfigFile
import DRcon.Paths
import DRcon.EvalParser
import DRcon.Prompt
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
    replLastCmd    :: Maybe InputType,
    replColor      :: Bool,
    replTimeout    :: Float,
    replEncoding   :: DecodeType,
    replPromp      :: (String, Prompt),
    replConnection :: RconConnection,
    replName       :: String
}


type Repl m = InputT (StateT ReplState m)


updateLastCmd :: (Monad m) => InputType -> Repl m ()
updateLastCmd cmd = lift $ modify (\s -> s {replLastCmd=Just cmd})


newPrompt :: String -> (String, Prompt)
newPrompt str = (str, parsePrompt str)


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


getInputLineWithPrompt :: Repl IO (Maybe String)
getInputLineWithPrompt = do
    state <- lift get
    let prompt = snd $ replPromp state
        con    = replConnection state
        name   = replName state

    prompt_vars <- liftIO $ getPromptVars name con
    getInputLine $ renderPrompt prompt_vars prompt


replLoop :: Repl IO ()
replLoop = handle (\Interrupt -> replLoop) $ withInterrupt $ do
    minput <- getInputLineWithPrompt
    case minput of
        Nothing    -> lift (replConnection <$> get) >>= liftIO . RCON.close
        Just input -> case parseCommand input of
            Left e -> do
                outputStrLn $ show e
                replLoop
            Right a -> replAction a


replAction :: InputType -> Repl IO ()
replAction cmd = case cmd of
    Empty -> replLoop
    Quit -> do
        con <- lift $ replConnection <$> get
        liftIO $ RCON.close con
    RepeatLast -> do
        last <- lift $ replLastCmd <$> get
        case last of
            Nothing -> outputStrLn noLastCmd >> replLoop
            Just last_cmd -> replAction last_cmd
    Help -> do
        outputStrLn $ T.unpack helpMessage
        updateLastCmd cmd
        replLoop
    ListVars -> do
        outputStrLn $ T.unpack helpVars
        updateLastCmd cmd
        replLoop
    Show var -> do
        con <- lift $ replConnection <$> get
        val <- showVar <$> case var of
            Mode -> liftIO $ SetMode <$> getMode con
            TimeDiff -> liftIO $ SetTimeDiff <$> getTimeDiff con
            Timeout -> lift $ SetTimeout . replTimeout <$> get
            Encoding -> lift $ SetEncoding . replEncoding <$> get
            Color -> lift $ SetColor . replColor <$> get
            PromptVar -> lift $ SetPrompt . fst . replPromp <$> get

        outputStrLn $ printf "%s: %s" (toTitle $ show var) val
        updateLastCmd cmd
        replLoop
    Set var_set -> do
        con <- lift $ replConnection <$> get
        case var_set of
            SetMode mode -> liftIO $ setMode con mode
            SetTimeDiff diff -> liftIO $ setTimeDiff con diff
            SetTimeout t -> lift $ modify (\s -> s {replTimeout=t})
            SetEncoding enc -> lift $ modify (\s -> s {replEncoding=enc})
            SetColor c -> lift $ modify (\s -> s {replColor=c})
            SetPrompt p -> lift $ modify (\s -> s {replPromp=newPrompt p})

        updateLastCmd cmd
        replLoop
    Version -> do
        outputStrLn versionInfo
        updateLastCmd cmd
        replLoop
    Login -> do
        pwd <- getPassword Nothing "Password: "
        con <- lift $ replConnection <$> get
        case pwd of
            (Just pass) -> liftIO $ RCON.setPassword con (BU.fromString pass)
            Nothing -> return ()
        replLoop
    History v -> do
        -- 10 is default value for history
        let num = fromMaybe 10 v
        history <- getHistory
        let h_lines = take num $ historyLines history
        forM_ h_lines $ outputStrLn . (replicate 2 ' ' ++)
        updateLastCmd cmd
        replLoop
    RconCommand command -> do
        rconEval $ TE.encodeUtf8 command
        updateLastCmd cmd
        replLoop
  where
    toTitle s = (\(f, e) -> map toUpper f ++ e) $ splitAt 1 s
    rconEval command = do
        repl_state <- lift get
        let time = replTimeout repl_state
            color = replColor repl_state
            enc = replEncoding repl_state
            con = replConnection repl_state

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
                                replTimeout=drconTimeout dargs,
                                replEncoding=drconEncoding dargs,
                                replPromp=prompt,
                                replConnection=con,
                                replName=connectName dargs}

    let hline_settings = setComplete comp base_settings
    evalStateT (runInputT hline_settings replLoop) repl_state
  where
    prompt = newPrompt "drcon %N> "
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
