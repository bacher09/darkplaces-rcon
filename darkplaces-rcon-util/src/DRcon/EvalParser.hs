{-# LANGUAGE OverloadedStrings #-}
module DRcon.EvalParser (
    InputType(..),
    parseCommand
) where

import Prelude hiding (break)
import Data.Char (isSpace)
import Data.Text


data InputType = Empty
               | Quit
               | Help
               | RepeatLast
               | History (Maybe Int)
               | UnknownCommand Text Text
               | RconCommand Text
    deriving(Show, Read, Eq)


parseCommand :: String -> InputType
parseCommand command
    | tcommand == empty = Empty
    | isPrefixOf ":" tcommand = parseInternalCommand cmd args
    | otherwise = RconCommand tcommand
  where
    tcommand = strip $ pack command
    (cmd, args) = break isSpace tcommand


parseInternalCommand :: Text -> Text -> InputType
parseInternalCommand ":quit" _ = Quit
parseInternalCommand ":help" _ = Help
parseInternalCommand ":?" _ = Help
parseInternalCommand ":" _ = RepeatLast
parseInternalCommand cmd args = UnknownCommand cmd args
