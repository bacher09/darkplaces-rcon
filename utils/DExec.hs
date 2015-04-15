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

#ifdef CABAL_VERSION
import Paths_darkplaces_rcon (version)
import Data.Version (showVersion)

versionStr = showVersion version
#else
versionStr = "dev"
#endif

versionStr :: String


data CommandArgs = CommandArgs {
    conArgs      :: ConnectionArgs,
    cliColor     :: Maybe Bool,
    cliCommand   :: String
} deriving(Show, Read, Eq)


toMicroseconds :: Float -> Int
toMicroseconds v = round $ v * 1e6


parseColorMode :: String -> ReadM (Maybe Bool)
parseColorMode mode_str
    | mode_str == "always" = return $ Just True
    | mode_str == "auto" = return Nothing
    | mode_str == "never" = return $ Just False
    | otherwise = readerError "value should be always, auto or never"


argsParser :: Parser CommandArgs
argsParser = CommandArgs
    <$> connParser
    <*> (option $ str >>= parseColorMode) (
        long "color"
        <> value Nothing
        <> help "Possible values are: `auto', `always' and `never'"
        <> metavar "COLOR_MODE")
    <*> (unwords <$> some
        (argument str (metavar "COMMAND"
                       <> help "Command that will be send to server")))


argsWithVersion :: Parser (Maybe CommandArgs)
argsWithVersion = flag' Nothing (
    long "version"
    <> short 'V'
    <> hidden) <|> (Just <$> argsParser)


rconExec :: RconInfo -> String -> Bool -> Float -> DecodeType -> IO ()
rconExec rcon command color time enc = do
    con <- RCON.connect rcon
    RCON.send con (BU.fromString command)
    printRecv con defaultStreamState
    RCON.close con
  where
    wait_time = toMicroseconds time
    printRecv con st = do
        mdata <- timeout wait_time $ RCON.recvRcon con
        let st_args = PrintStreamArgs {
            withColor=color,
            streamState=st,
            decodeFun=toUTF enc}

        case mdata of
            (Just r) -> printStreamDPText st_args (BL.fromStrict r) >>= printRecv con
            Nothing -> streamEnd color st


processArgs :: Maybe CommandArgs -> UtilError ()
processArgs Nothing = liftIO $ putStrLn $ "Version: " ++ versionStr
processArgs (Just args) = do
    (rcon, time_out, enc) <- rconConfigure $ conArgs args
    color <- liftIO $ case cliColor args of
        (Just c) -> return c
        Nothing -> supportColors

    liftIO $ rconExec rcon command color time_out enc
  where
    command = cliCommand args


main :: IO ()
main = handleErrors . processArgs =<< execParser opts
  where
    opts = info (helper <*> argsWithVersion)
        (fullDesc <> progDesc "Darkplaces rcon client utility")
    handleErrors me = do
        r <- runErrorT me
        case r of
            (Right ()) -> return ()
            (Left e) -> putStrLn e
