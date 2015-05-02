module DRcon.Util (
    UtilError,
    defaultConnectionArgs,
    mergeConnectionArgs,
    connParser,
    configName,
    configPath,
    readConfig,
    rconConfigure,
) where
import Data.Either
import System.Exit

