{-# LANGUAGE OverloadedStrings #-}
module DRcon.EvalParser (
    InputType(..),
    parseCommand
) where

import Prelude hiding (break)
import Data.Char (isSpace)
import Data.Maybe
import Data.Text hiding (filter)
import Data.Text.Read


data InputType = Empty
               | Quit
               | Help
               | RepeatLast
               | History (Maybe Int)
               | WrongArgument Text Text
               | UnknownCommand Text Text
               | RconCommand Text
    deriving(Show, Read, Eq)


disambiguate :: Text -> Maybe Text
disambiguate cmd = case search_cmds of
    [new_command] -> Just new_command
    _             -> Nothing
  where
    search_cmds = filter (isPrefixOf cmd) commands
    commands = [":quit", ":help", ":history"]


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
parseInternalCommand ":history" "" = History Nothing
parseInternalCommand ":history" v = case decimal v of
    (Right (num, "")) -> History (Just num)
    _                 -> WrongArgument ":history" v
parseInternalCommand cmd args = UnknownCommand cmd args
