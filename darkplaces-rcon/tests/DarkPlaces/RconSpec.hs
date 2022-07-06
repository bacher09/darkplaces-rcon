{-# LANGUAGE CPP #-}
module DarkPlaces.RconSpec (
    spec
) where
import Test.Hspec
#ifdef INTEGRATION
import System.Timeout
import Control.Monad (void)
import Data.Maybe
import DarkPlaces.Rcon
import qualified Data.ByteString as B
#else
import qualified Test.Hspec.Core.Spec as HC
#endif


spec :: Spec
#ifdef INTEGRATION
spec = do
    describe "connect" $ do
        let rcon = makeRcon "localhost" "26000" "test"
        it "should answer to status command" $ do
            c <- connect rcon
            checkResponse c NonSecureRcon
            checkResponse c TimeSecureRcon
            checkResponse c ChallengeSecureRcon
            checkResponse c ChallengeSecureRcon
            checkResponse c ChallengeSecureRcon
            close c
  where
    checkResponse c mode = do
        setMode c mode
        void $ send c "status"
        r <- timeout 1500000 $ recv c
        (fromJust r) `shouldSatisfy` B.isPrefixOf "host:"
#else
spec = HC.fromSpecList []
#endif
