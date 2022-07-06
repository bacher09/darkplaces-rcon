{-# LANGUAGE OverloadedStrings #-}
module DRcon.ConfigFileSpec (
    spec
) where
import Test.Hspec
import DRcon.CommandArgs
import DRcon.ConfigFile
import Data.ConfigFile
import Data.Either
import DarkPlaces.Rcon


testConf1 :: String
testConf1 = "[DEFAULT]\n\
\prompt=\"%{[0;32m%}%P %{[1;34m%}%N> %{[0m%}\"\n\
\\n\
\[server]\n\
\server=someserver.com\n\
\password=somepwd\n\
\challengetimeout=50\n\
\challengeretries=15\n\
\mode=1\n"


spec :: Spec
spec = do
    describe "parseConfigFile" $ do
        it "valid config" $ do
            let econf = readstring configParser testConf1 >>= (`argsFromConfig` "server")
            econf `shouldSatisfy` isRight
            case econf of
              Right t -> checkConf1 t
              Left _ -> return ()
  where
    checkConf1 conf = do
      confServerString conf `shouldBe` "someserver.com"
      confPassword conf `shouldBe` Just "somepwd"
      confChallengeTimeout conf `shouldBe` Just 50
      confChallengeRetries conf `shouldBe` Just 15
      confMode conf `shouldBe` Just TimeSecureRcon
