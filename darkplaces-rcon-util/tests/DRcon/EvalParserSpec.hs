{-# LANGUAGE OverloadedStrings #-}
module DRcon.EvalParserSpec (
    spec
) where
import Test.Hspec
import DRcon.EvalParser
import DarkPlaces.Rcon


spec :: Spec
spec = do
    describe "parseCommand" $ do
        it "check simple rcon commands" $ do
            "status" `cmdShouldBe` RconCommand "status"
            "sv_cmd lsmaps" `cmdShouldBe` RconCommand "sv_cmd lsmaps"
            "ls *.bsp" `cmdShouldBe` RconCommand "ls *.bsp"

        it "check empty input" $ do
            "" `cmdShouldBe` Empty
            -- input string with spaces
            " "      `cmdShouldBe` Empty
            "  "     `cmdShouldBe` Empty
            "\n  "   `cmdShouldBe` Empty
            "\n  \t" `cmdShouldBe` Empty

        it "check :quit internal command" $ do
            ":q" `cmdShouldBe` Quit
            ":qu" `cmdShouldBe` Quit
            ":qui" `cmdShouldBe` Quit
            ":quit" `cmdShouldBe` Quit
            -- quit accept any arguments
            ":q some vals" `cmdShouldBe` Quit

        it "check :help internal command" $ do
            ":?" `cmdShouldBe` Help
            ":h" `cmdShouldBe` Help
            ":he" `cmdShouldBe` Help
            ":help" `cmdShouldBe` Help
            -- help takes no argumens
            ":help haha" `cmdShouldErr` (CommandTakesNoArgumens ":help" "haha")

        it "check : internal command" $ do
            ":" `cmdShouldBe` RepeatLast
            ": " `cmdShouldBe` RepeatLast
            " : " `cmdShouldBe` RepeatLast
            "\t :" `cmdShouldBe` RepeatLast
            -- : takes no argumens
            ": help" `cmdShouldErr` (CommandTakesNoArgumens ":" "help")

        it "check :login internal command" $ do
            ":login" `cmdShouldBe` Login
            " :l" `cmdShouldBe` Login
            ":lo" `cmdShouldBe` Login
            " :log " `cmdShouldBe` Login
            ":l ogin" `cmdShouldErr` (CommandTakesNoArgumens ":login" "ogin")

        it "check :version internal command" $ do
            ":version" `cmdShouldBe` Version

        it "check :history internal command" $ do
            ":hi" `cmdShouldBe` (History Nothing)
            ":his" `cmdShouldBe` (History Nothing)
            ":history" `cmdShouldBe` (History Nothing)
            ":his 10" `cmdShouldBe` (History $ Just 10)
            ":history 12 " `cmdShouldBe` (History $ Just 12)
            ":history -2 " `cmdShouldErr` (WrongArgumentType ":history" "-2")
            ":history 2 4 " `cmdShouldErr` (WrongArgumentType ":history" "2 4")
            ":history 2.4 " `cmdShouldErr` (WrongArgumentType ":history" "2.4")
            ":history test " `cmdShouldErr` (WrongArgumentType ":history" "test")

        it "check :set internal command" $ do
            ":set" `cmdShouldBe` ListVars
            ":s" `cmdShouldBe` ListVars
            ":set mode" `cmdShouldBe` (Show Mode)
            ":set mode 2" `cmdShouldBe` (Set $ SetMode ChallangeSecureRcon)
            ":set mode 1" `cmdShouldBe` (Set $ SetMode TimeSecureRcon)
            ":set mode 0" `cmdShouldBe` (Set $ SetMode NonSecureRcon)
  where
    cmdShouldBe cmd_str cmd_res = parseCommand cmd_str `shouldBe` (Right cmd_res)
    cmdShouldErr cmd_str cmd_res = parseCommand cmd_str `shouldBe` (Left cmd_res)
