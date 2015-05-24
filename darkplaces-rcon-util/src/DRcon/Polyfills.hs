module DRcon.Polyfills (
    readMaybe,
    lookupEnv,
    defaultTimeLocale
) where


#if MIN_VERSION_time(1,5,0)
import Data.Time.Format(defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif


#if MIN_VERSION_base(4,6,0)
import Text.Read (readMaybe)
import System.Environment (lookupEnv)
#else
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catchJust)
import Control.Monad (guard)
import System.Environment (getEnv)

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing


lookupEnv :: String -> IO (Maybe String)
lookupEnv name = catchJust (guard . isDoesNotExistError) (Just <$> getEnv name) (\e -> return Nothing)
#endif
