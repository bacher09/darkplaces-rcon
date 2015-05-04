{-# LANGUAGE OverloadedStrings #-}
module DRcon.EvalParser (
    InputType(..),
    parseCommand,
    helpMessage
) where

import Prelude hiding (break)
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
               | WrongArgument Text Text
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


helpMessage :: Text
helpMessage = " Available commands:\n\n" `append` cmdList
  where
    cmdList = T.concat $ map (uncurry formatCmd) cmdInfo
    formatCmd name info = T.concat [
        T.replicate 3 " ",
        justifyLeft 28 ' ' name,
        info, "\n"]


disambiguate :: Text -> Maybe Text
disambiguate cmd = case search_cmds of
    [new_command] -> Just new_command
    _             -> Nothing
  where
    search_cmds = filter (isPrefixOf cmd) commands
    commands = [":quit", ":help", ":history", ":version"]


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
parseInternalCommand ":help" _ = Help
parseInternalCommand ":h" _ = Help
parseInternalCommand ":?" _ = Help
parseInternalCommand ":" _ = RepeatLast
parseInternalCommand ":version" _ = Version
parseInternalCommand ":history" "" = History Nothing
parseInternalCommand ":history" v = case decimal v of
    (Right (num, "")) -> History (Just num)
    _                 -> WrongArgument ":history" v
parseInternalCommand cmd args = UnknownCommand cmd args
