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
    <*> argument str (
        metavar "COMMAND"
        <> help "Command to execute")


rconExec :: RconInfo -> String -> Float -> IO ()
rconExec rcon command time = do
    con <- RCON.connect rcon
    RCON.send con (BU.fromString command)
    printRecv con
    RCON.close con
  where
    wait_time = toMicroseconds time
    printRecv con = do
        mdata <- timeout wait_time $ RCON.recvRcon con
        case mdata of
            (Just r) -> printDPText False (BL.fromStrict r) >> printRecv con
            Nothing -> return ()



{-rconClient :: CommandArgs -> IO ()-}
{-rconClient args = do-}
    {-[>let (Just server) = defaultHostAndPort "26000" $ serverString args<]-}
    {-[>let brcon = makeRcon (fst server) (snd server) (BU.fromString $ cliPassword args)<]-}
    {-[>let rcon = brcon {rconMode=cliMode args, rconTimeDiff=cliTimeDiff args}<]-}
    {-[>print rcon<]-}
    {-print args-}
    {-config_file <- configPath-}
    {-mcon <- case eitherGetArgs mconf server_str of-}
        {-(Right r) -> return r-}
        {-(Left e) -> print e >> exitFailure-}

    {-print mcon-}

    {-[>con <- RCON.connect rcon<]-}
    {-[>let command = BU.fromString $ cliCommand args<]-}
    {-[>RCON.send con command<]-}
    {-[>printRecv con<]-}
  {-where-}
    {-argsConn = conArgs args-}
    {-server_str = fromMaybe "DEFAULT" $ conServerString $ argsConn-}
    {-[>time_wait = toMicroseconds $ cliTimeout args<]-}
    {-[>time_wait = toMicroseconds $ cliTimeout args<]-}
    {-[>printRecv con = do<]-}
        {-[>mdata <- timeout time_wait $ RCON.recvRcon con<]-}
        {-[>case mdata of<]-}
            {-[>(Just r) -> printDPText False (BL.fromStrict r) >> printRecv con<]-}
            {-[>Nothing -> return ()<]-}


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
