module DRcon.Paths (
    configName,
    configDirPath,
    configPath,
    historyPath
) where

import System.Directory (getHomeDirectory, getAppUserDataDirectory)
import System.FilePath
import DRcon.Polyfills (lookupEnv)


configName :: String
configName = "drcon.ini"


configDirPath :: IO String
configDirPath = do
    h_path <- lookupEnv "DRCON_HOME"
    case h_path of
        (Just path) -> return path
        Nothing -> getAppUserDataDirectory "drcon"


configPath :: IO String
configPath = do
    path <- configDirPath
    return $ path </> configName


historyPath :: IO String
historyPath = do
    m_path <- lookupEnv "DRCON_HISTFILE"
    config_dir <- configDirPath
    case m_path of
        (Just path) -> return path
        Nothing -> return $ config_dir </> "drcon_history"
