module Main where
import DarkPlaces.Rcon.Util
import DarkPlaces.Rcon hiding (connect, send)
import qualified DarkPlaces.Rcon as RCON
import DarkPlaces.Text
import Options.Applicative
import Network.HostAndPort (defaultHostAndPort)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import System.Timeout
import qualified Data.ByteString.UTF8 as BU
import Data.ConfigFile
import Data.Maybe
import System.Exit
import Control.Monad.Error


data CommandArgs = CommandArgs {
    conArgs      :: ConnectionArgs,
    cliCommand   :: String
} deriving(Show, Read, Eq)


toMicroseconds :: Float -> Int
toMicroseconds v = round $ v * 1e6


argsParser :: Parser CommandArgs
argsParser = CommandArgs
    <$> connParser
    <*> (unwords <$> some
        (argument str (metavar "COMMAND"
                       <> help "Command to execute")))


rconExec :: RconInfo -> String -> Float -> IO ()
rconExec rcon command time = do
    con <- RCON.connect rcon
    RCON.send con (BU.fromString command)
    printRecv con defaultStreamState
    RCON.close con
  where
    wait_time = toMicroseconds time
    printRecv con st = do
        mdata <- timeout wait_time $ RCON.recvRcon con
        case mdata of
            (Just r) -> printStreamDPText False st (BL.fromStrict r) >>= printRecv con
            Nothing -> streamEnd True st


processArgs :: CommandArgs -> UtilError ()
processArgs args = do
    (rcon, time_out) <- rconConfigure $ conArgs args
    liftIO $ rconExec rcon command time_out
  where
    command = cliCommand args


main :: IO ()
main = handleErrors . processArgs =<< execParser opts
  where
    opts = info (helper <*> argsParser)
        (fullDesc <> progDesc "Darkplaces rcon client")
    handleErrors me = do
        r <- runErrorT me
        case r of
            (Right ()) -> return ()
            (Left e) -> putStrLn e
