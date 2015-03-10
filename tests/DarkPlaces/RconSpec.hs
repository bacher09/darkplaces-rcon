{-# LANGUAGE CPP #-}
module DarkPlaces.RconSpec (
    spec
) where
import Test.Hspec
#ifdef INTEGRATION
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
            checkResponse c ChallangeSecureRcon
            isConnected c `shouldReturn` True
            close c
  where
    checkResponse c mode = do
        setMode c mode
        send c "status"
        r <- recvRcon c
        r `shouldSatisfy` B.isPrefixOf "host:"
#else
spec = HC.fromSpecList []
#endif
