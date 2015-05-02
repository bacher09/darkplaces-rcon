module Main where
import DRcon.CommandArgs
import DRcon.ConfigFile
import DRcon.Paths
import DarkPlaces.Rcon hiding (connect, send)
import qualified DarkPlaces.Rcon as RCON
import DarkPlaces.Text
import Options.Applicative
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import System.Timeout
import qualified Data.ByteString.UTF8 as BU
import Data.ConfigFile
import Data.Maybe
import System.Exit
import Control.Monad.Error
import System.Console.Haskeline

#ifdef CABAL_VERSION
import Paths_darkplaces_rcon_util (version)
import Data.Version (showVersion)

versionStr = showVersion version
#else
versionStr = "dev"
#endif

versionStr :: String


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


rconExec :: RconInfo -> String -> Bool -> Float -> DecodeType -> IO ()
rconExec rcon command color time enc = do
    con <- RCON.connect rcon
    RCON.send con (BU.fromString command)
    printRecv con (time, color, enc) defaultStreamState
    RCON.close con


rconRepl :: RconInfo -> Bool -> Float -> DecodeType -> IO ()
rconRepl rcon color time enc = do
    con <- RCON.connect rcon
    hist_path <- historyPath
    let hline_settings = defaultSettings {historyFile=Just hist_path,
                                         autoAddHistory=True}
    runInputT hline_settings $ loop con
  where
    loop con = handle (\Interrupt -> loop con) $ withInterrupt $ do
        minput <- getInputLine "> "
        case minput of
            Nothing -> liftIO $ RCON.close con
            Just input -> do
                liftIO $ RCON.send con (BU.fromString input)
                liftIO $ printRecv con (time, color, enc) defaultStreamState
                loop con


processArgs :: Maybe CommandArgs -> UtilError ()
processArgs Nothing = liftIO $ putStrLn $ "Version: " ++ versionStr
processArgs (Just args) = do
    (rcon, time_out, enc) <- rconConfigure $ conArgs args
    color <- liftIO $ case cliColor args of
        (Just c) -> return c
        Nothing -> supportColors

    case cliCommand args of
        (Just command) -> liftIO $ rconExec rcon command color time_out enc
        Nothing -> liftIO $ rconRepl rcon color time_out enc
  where
    command = cliCommand args


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
