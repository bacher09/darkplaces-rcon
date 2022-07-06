{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where
import DRcon.CommandArgs
import DRcon.ConfigFile
import DRcon.Paths
import DRcon.EvalParser
import DRcon.Prompt
import DRcon.Version (versionInfo)
import DRcon.ConsoleWrapper (decoratedConsole)
import DarkPlaces.Rcon hiding (connect, send, getPassword)
import qualified DarkPlaces.Rcon as RCON
import DarkPlaces.Text
import Options.Applicative
import System.Timeout
import qualified Data.ByteString.UTF8 as BU
import Control.Monad.Except
import System.Console.Haskeline
import System.Console.Haskeline.History (historyLines)
import Control.Monad.Reader
import Control.Monad.State.Class
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding as TE
import Text.Printf
import Data.Char (toUpper)
import Data.Conduit
import Control.Monad.Catch (handle, bracket, MonadMask, MonadCatch, MonadThrow)
import qualified System.IO as IO
import Control.Concurrent (forkIO, killThread)
import Data.IORef


data ReplState = ReplState {
    replLastCmd    :: Maybe InputType,
    replColor      :: Bool,
    replTimeout    :: Float,
    replEncoding   :: DecodeType,
    replPromp      :: (String, Prompt),
    replConnection :: RconConnection,
    replName       :: String
}

newtype StateRefT s m a = StateRefT (ReaderT (IORef s) m a)
    deriving(Functor, Applicative, Monad, MonadIO, MonadTrans, MonadThrow, MonadCatch, MonadMask)


instance MonadIO m => MonadState s (StateRefT s m) where
    get = StateRefT $ ReaderT $ liftIO . readIORef
    put x = StateRefT $ ReaderT $ \ref -> liftIO $ atomicWriteIORef ref $! x


atomicModify :: (MonadIO m) => (s -> s) -> StateRefT s m ()
atomicModify f = StateRefT $ ReaderT $ \ref -> liftIO $ atomicModifyIORef' ref (\x -> (f x, ()))

getRef :: (Monad m) => StateRefT s m (IORef s)
getRef = StateRefT ask

type Repl m a = InputT (StateRefT ReplState m) a
runStateRefT :: (MonadIO m) => s -> StateRefT s m a -> m a
runStateRefT state (StateRefT fun) = do
    ref <- liftIO $ newIORef state
    liftIO $ writeIORef ref state
    runReaderT fun ref


runRepl :: (MonadIO m, MonadMask m) => ReplState -> Settings (StateRefT ReplState m) -> Repl m a -> m a
runRepl state settings repl = runStateRefT state (runInputT settings repl)


updateLastCmd :: (MonadIO m) => InputType -> Repl m ()
updateLastCmd cmd = lift $ atomicModify (\s -> s {replLastCmd=Just cmd})


newPrompt :: String -> (String, Prompt)
newPrompt str = (str, parsePrompt str)


toMicroseconds :: Float -> Int
toMicroseconds v = round $ v * 1e6


rconTimeoutConduit :: RconConnection -> Int -> ConduitT () BU.ByteString IO ()
rconTimeoutConduit con t = do
    mdata <- liftIO $ timeout t $ RCON.recv con
    case mdata of
        (Just r) -> yield r >> rconTimeoutConduit con t
        Nothing -> return ()


rconExec :: DRconArgs -> String -> Bool -> IO ()
rconExec dargs command color = bracket (RCON.connect $ connectInfo dargs) RCON.close rconIO
  where
    time = drconTimeout dargs
    enc = drconEncoding dargs
    output = if color then hOutputColorsLn else hOutputNoColorsLn
    outStream = parseDPText .| toUTF enc .| output IO.stdout
    rconIO con = do
        void $ RCON.send con (BU.fromString command)
        runConduit $ rconTimeoutConduit con (toMicroseconds time) .| outStream


getInputLineWithPrompt :: Repl IO (Maybe String)
getInputLineWithPrompt = do
    state <- lift get
    let prompt = snd $ replPromp state
        con    = replConnection state
        name   = replName state

    prompt_vars <- liftIO $ getPromptVars name con
    getInputLine $ renderPrompt prompt_vars prompt


dpLineConduit :: (Eq a) => DPTextFilter a m a
dpLineConduit = do
    t <- await
    case t of
        Just v -> yield v >> unless (v == DPNewline) dpLineConduit
        Nothing -> return ()


replStart :: Repl IO ()
replStart = bracket outputLoopStart (liftIO . killThread) $ const replLoop
  where
    outputLoopStart = do
        con <- lift $ gets replConnection
        handle <- getExternalPrint >>= liftIO . decoratedConsole IO.stdout
        ref <- lift getRef
        let dpStream = rconTimeoutConduit con (-1) .| parseDPText
        liftIO $ forkIO $ runConduit $ dpStream .| fix (\loop -> do
            state <- liftIO $ readIORef ref
            let color = replColor state
                enc = replEncoding state
                output = if color then hOutputColors else hOutputNoColors
            dpLineConduit .| toUTF enc .| output handle >> loop)


replLoop :: Repl IO ()
replLoop = handle (\Interrupt -> replLoop) $ withInterrupt $ do
    minput <- getInputLineWithPrompt
    case minput of
        Nothing    -> return ()
        Just input -> case parseCommand input of
            Left e -> do
                outputStrLn $ show e
                replLoop
            Right a -> replAction a


replAction :: InputType -> Repl IO ()
replAction cmd = case cmd of
    Empty -> replLoop
    Quit -> return ()
    RepeatLast -> do
        last <- replLastCmd <$> lift get
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
        con <- replConnection <$> lift get
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
        con <- replConnection <$> lift get
        case var_set of
            SetMode mode -> liftIO $ setMode con mode
            SetTimeDiff diff -> liftIO $ setTimeDiff con diff
            SetTimeout t -> lift $ atomicModify (\s -> s {replTimeout=t})
            SetEncoding enc -> lift $ atomicModify (\s -> s {replEncoding=enc})
            SetColor c -> lift $ atomicModify (\s -> s {replColor=c})
            SetPrompt p -> lift $ atomicModify (\s -> s {replPromp=newPrompt p})

        updateLastCmd cmd
        replLoop
    Version -> do
        outputStrLn versionInfo
        updateLastCmd cmd
        replLoop
    Login -> do
        pwd <- getPassword Nothing "Password: "
        con <- replConnection <$> lift get
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
    rconEval :: BU.ByteString -> Repl IO ()
    rconEval command = do
        -- TODO: implement sending thread
        con <- lift $  gets replConnection
        void $ liftIO $ RCON.send con command

    noLastCmd = "there is no last command to perform\n" ++
        "use :? for help."


rconRepl :: DRconArgs -> Bool -> IO ()
rconRepl dargs color = bracket (RCON.connect $ connectInfo dargs) RCON.close launchRepl
  where
    compliter prev cmd = return $
        simpleCompletion . T.unpack <$> internalAutoComplete (T.pack prev) (T.pack cmd)
    comp = completeWordWithPrev Nothing " \t" compliter
    launchRepl con = do
        hist_path <- historyPath
        let base_settings = defaultSettings {historyFile=Just hist_path, autoAddHistory=True}
            repl_state = ReplState {
                replLastCmd=Nothing,
                replColor=color,
                replTimeout=drconTimeout dargs,
                replEncoding=drconEncoding dargs,
                replPromp=newPrompt $ drconPrompt dargs,
                replConnection=con,
                replName=connectName dargs
            }

        let hline_settings = setComplete comp base_settings
        runRepl repl_state hline_settings replStart


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
        r <- runExceptT me
        case r of
            (Right ()) -> return ()
            (Left e) -> putStrLn e
