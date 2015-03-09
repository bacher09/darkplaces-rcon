module DarkPlaces.RconSpec (
    spec
) where
import Test.Hspec
import DarkPlaces.Rcon
import DarkPlaces.Rcon.Internal


spec :: Spec
spec = do
    describe "rconNonSecurePacket" $ do
        it "when password is passw and command status" $ do
            let res = "\xFF\xFF\xFF\xFFrcon passw status"
            rconNonSecurePacket "passw" "status" `shouldBe` res

    describe "rconSecureTimePacket" $ do
        it "when time is 100.0, password is passw and command status" $ do
            let res = "\xff\xff\xff\xffsrcon HMAC-MD4 TIME \
                      \R\xcbv\xf0\xa7p\xcd\xca\xf2!\xc3~\x06\
                      \\xa9\x9f\xa8 100.000000 status"

            rconSecureTimePacket (100.0 :: Float) "passw" "status" `shouldBe` res

    describe "rconSecureChallangePacket" $ do
        it "when challange is 11111111111 password is passw and command status" $ do
            let res = "\xff\xff\xff\xffsrcon HMAC-MD4 CHALLENGE \
                      \D\x89\xfd\x15\xccZ\xea\xeb\x0e\xbfl\xd6\&C\
                      \\x05T\x12 11111111111 status"
            rconSecureChallangePacket "11111111111" "passw" "status" `shouldBe` res

    describe "parseChallenge" $ do
        it "valid packet return challange" $ do
            let resp = "\xff\xff\xff\xff\&challenge 11111111111\x00vlen.\
                       \\x00\x00\x00d0pkXon//KssdlzGkFKdnnN4sgg8H+koTb\
                       \Bn5JTi37BAW1t=\x00\x00"

            parseChallenge resp `shouldBe` Just "11111111111"

        it "invalid packet should return nothing" $ do
            parseChallenge "bad packet" `shouldBe` Nothing
