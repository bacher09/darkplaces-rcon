module DRcon.EvalParser (
    InputType(..),
    parseCommand
) where

import Data.Char (isSpace)
import Data.Maybe


data InputType = Empty
               | Quit
               | Help
               | RepeatLast
               | History (Maybe Int)
               | UnknownCommand String String
               | RconCommand String
    deriving(Show, Read, Eq)


parseCommand :: String -> InputType
parseCommand command = case mfirstChar of
    Nothing -> Empty
    (Just ':') -> parseInternalCommand (head $ words command) command
    (Just _) -> RconCommand command
  where
    mfirstChar = listToMaybe $ filter (not . isSpace) command


parseInternalCommand :: String -> String -> InputType
parseInternalCommand ":quit" _ = Quit
parseInternalCommand ":help" _ = Help
parseInternalCommand ":?" _ = Help
parseInternalCommand ":" _ = RepeatLast
parseInternalCommand cmd args = UnknownCommand cmd args
