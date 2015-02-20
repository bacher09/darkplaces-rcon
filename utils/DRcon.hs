module Main where
import DarkPlaces.Rcon hiding (connect, send)
import qualified DarkPlaces.Rcon as RCON
import Options.Applicative


#if __GLASGOW_HASKELL__ >= 706
import Text.Read (readMaybe)
#else

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

#endif


data CommandArgs = CommandArgs {
    serverString :: String,
    rconPassword :: String,
    rconMode     :: RconMode,
    timeDiff     :: Int,
    timeout      :: Float
} deriving(Show, Read, Eq)



parseRconMode :: String -> ReadM RconMode
parseRconMode ms = case readMaybe ms of
    Just x | x >= 0 && x < 3 -> return $ toEnum x
    _ -> readerError "value should be 0, 1 or 2"


argsParser :: Parser CommandArgs
argsParser = CommandArgs
    <$> strOption (
        short 's'
        <> long "server"
        <> metavar "SERVER"
        <> help "Server to connect")
    <*> strOption (
        short 'p'
        <> long "password"
        <> metavar "PASSWORD")
    <*> option (str >>= parseRconMode) (
        short 'm'
        <> long "mode"
        <> value TimeSecureRcon
        <> metavar "MODE")
    <*> option auto (
        short 'd'
        <> long "time-diff"
        <> value 0
        <> metavar "TIMEDIFF")
    <*> option auto (
        short 't'
        <> long "timeout"
        <> metavar "TIMEOUT"
        <> value 1.6)


rconClient :: CommandArgs -> IO ()
rconClient args = do
    print args


main :: IO ()
main = rconClient =<< execParser opts
  where
    opts = info (helper <*> argsParser)
        (fullDesc <> progDesc "Darkplaces rcon client")
