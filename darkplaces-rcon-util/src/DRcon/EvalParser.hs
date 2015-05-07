{-# LANGUAGE OverloadedStrings #-}
module DRcon.EvalParser (
    InputType(..),
    parseCommand,
    internalAutoComplete,
    helpMessage
) where

import Prelude hiding (break, concat)
import Data.Char (isSpace)
import Data.Maybe
import Data.Text hiding (filter, map)
import qualified Data.Text as T
import Data.Text.Read


data InputType = Empty
               | Quit
               | Help
               | Version
               | RepeatLast
               | History (Maybe Int)
               | WrongArgument Text Text Text
               | UnknownCommand Text Text
               | RconCommand Text
    deriving(Show, Read, Eq)


cmdInfo :: [(Text, Text)]
cmdInfo = [
    ("<rcon command>", "execute command via rcon"),
    (":", "repeat last command"),
    (":help, :?", "display list of commands"),
    (":history [n]", "show n (10 by default) last commands"),
    (":quit", "quit from drcon"),
    (":version", "print program version")]


topLevelCommands :: [Text]
topLevelCommands = [":", ":?", ":quit", ":help", ":history", ":version"]


helpMessage :: Text
helpMessage = " Available commands:\n\n" `append` cmdList
  where
    cmdList = T.concat $ map (uncurry formatCmd) cmdInfo
    formatCmd name info = T.concat [
        T.replicate 3 " ",
        justifyLeft 28 ' ' name,
        info, "\n"]


disambiguate :: Text -> Maybe Text
disambiguate cmd
    | T.length cmd > 1 = listToMaybe search_cmds
    | otherwise = Nothing
  where
    search_cmds = filter (isPrefixOf cmd) commandPriorities
    -- list of disambiguateble commands in priority order
    commandPriorities = [ ":help", ":quit", ":history", ":version"]


internalAutoComplete :: Text -> Text -> [Text]
internalAutoComplete prev cmd = case prev_cmd of
    "" -> filter (isPrefixOf cmd) topLevelCommands
    _  -> []
  where
    sprev = strip prev
    prev_cmd = fromMaybe sprev $ disambiguate sprev


formatWrongArgument :: Text -> Text -> Text
formatWrongArgument ":history" _ = "Wrong argument\nSyntax:  :history [n]"
formatWrongArgument cmd _ = concat [
    "Wrong argument for command \"", cmd, "\""]


parseCommand :: String -> InputType
parseCommand command
    | tcommand == empty = Empty
    | isPrefixOf ":" tcommand = parseInternalCommand dcmd (strip args)
    | otherwise = RconCommand tcommand
  where
    tcommand = strip $ pack command
    (cmd, args) = break isSpace tcommand
    dcmd = fromMaybe cmd $ disambiguate cmd


parseInternalCommand :: Text -> Text -> InputType
parseInternalCommand ":quit" _ = Quit
parseInternalCommand ":help" "" = Help
parseInternalCommand ":?" "" = Help
parseInternalCommand ":" "" = RepeatLast
parseInternalCommand ":version" "" = Version
parseInternalCommand ":history" "" = History Nothing
parseInternalCommand ":history" v = case decimal v of
    (Right (num, "")) -> History (Just num)
    _                 -> WrongArgument ":history" v
                            (formatWrongArgument ":history" v)
parseInternalCommand cmd args
    | cmd `elem` withoutArgs = WrongArgument cmd args noArgsMsg
    | otherwise = UnknownCommand cmd args
  where
    withoutArgs = [":", ":help", ":?", ":version"]
    noArgsMsg = concat ["Error:  command \"", cmd,"\" takes no arguments"]
