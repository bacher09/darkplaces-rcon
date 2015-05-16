{-# LANGUAGE OverloadedStrings #-}
module DRcon.EvalParser (
    CommandErrorType(..),
    EvalVar(..),
    VarValue(..),
    InputType(..),
    EvalCmd,
    parseCommand,
    internalAutoComplete,
    helpMessage,
    helpVars,
    showVar
) where

import Prelude hiding (break, concat)
import Data.Char (isSpace)
import Data.Maybe
import Data.Text hiding (filter, map)
import qualified Data.Text as T
import Data.Text.Read
import Control.Monad.Error
import Control.Applicative ((<$>))
import DRcon.CommandArgs (parseRconMode, parseEncoding, showEncoding)
import DRcon.Prompt (readPrompt)
import DarkPlaces.Rcon (RconMode)
import DarkPlaces.Text (DecodeType(..))
import Data.Monoid
import Text.Printf


data CommandErrorType = UnknownCommand Text
                      | WrongArgumentType Text Text
                      | CommandTakesNoArgumens Text Text
                      | OtherError Text
    deriving(Eq)


data EvalVar = Mode
             | TimeDiff
             | Timeout
             | Encoding
             | Color
             | PromptVar
    deriving(Eq, Ord, Bounded, Enum)


data VarValue = SetMode RconMode
              | SetTimeDiff Int
              | SetTimeout Float
              | SetEncoding DecodeType
              | SetColor Bool
              | SetPrompt String
    deriving(Eq)


data InputType = Empty
               | Quit
               | Help
               | Version
               | RepeatLast
               | Login
               | ListVars
               | Show EvalVar
               | Set VarValue
               | History (Maybe Int)
               | RconCommand Text
    deriving(Show, Eq)


type EvalCmd = Either CommandErrorType InputType


instance Show CommandErrorType where
    show (UnknownCommand cmd) = printf badCmd $ unpack cmd
      where
        badCmd = "unknown command '%s'\nuse :? for help."

    show (WrongArgumentType cmd _) = case cmd of
        ":history" -> "Wrong argument\nSyntax:  :history [n]"
        _          -> printf "Wrong argument for command \"%s\"" $ unpack cmd

    show (CommandTakesNoArgumens cmd _) = printf noArgs $ unpack cmd
      where
        noArgs = "Error:  command \"%s\" takes no arguments"

    show (OtherError msg) = unpack msg


instance Show EvalVar where
    show Mode = "mode"
    show TimeDiff = "timediff"
    show Timeout = "timeout"
    show Encoding = "encoding"
    show Color = "color"
    show PromptVar = "prompt"


instance Show VarValue where
    show v = show (getVarName v) ++ " " ++ showVar v


cmdInfo :: [(Text, Text)]
cmdInfo = [
    ("<rcon command>", "execute command via rcon"),
    (":", "repeat last command"),
    (":help, :?", "display list of commands"),
    (":history [n]", "show n (10 by default) last commands"),
    (":login", "promp for server password"),
    (":quit", "quit from drcon"),
    (":version", "print program version")]


-- list of commands in priority order
topLevelCommands :: [Text]
topLevelCommands = [":help", ":quit", ":set", ":login",
                    ":history", ":version", ":", ":?"]


helpMessage :: Text
helpMessage = " Available commands:\n\n" <> cmdList
  where
    cmdList = T.concat $ map (uncurry formatCmd) cmdInfo
    formatCmd name info = T.concat [
        T.replicate 3 " ",
        justifyLeft 28 ' ' name,
        info, "\n"]


helpVars :: Text
helpVars = " Available vars:\n\n" <> varsList
  where
    varsList = intercalate "\n" $ map (T.replicate 3 " " `append`) varsText
    vars :: [EvalVar]
    vars = enumFromTo minBound maxBound
    varsText = map (pack . show) vars


parseEvalVar :: Text -> Maybe EvalVar
parseEvalVar var = case var of
    "mode"     -> Just Mode
    "timediff" -> Just TimeDiff
    "timeout"  -> Just Timeout
    "encoding" -> Just Encoding
    "color"    -> Just Color
    "prompt"   -> Just PromptVar
    _          -> Nothing


showBool :: Bool -> String
showBool True = "yes"
showBool False = "no"


showVar :: VarValue -> String
showVar (SetMode m) = show $ fromEnum m
showVar (SetTimeDiff d) = show d
showVar (SetTimeout tm) = show tm
showVar (SetEncoding enc) = showEncoding enc
showVar (SetColor c) = showBool c
showVar (SetPrompt p) = show p


getVarName :: VarValue -> EvalVar
getVarName (SetMode _) = Mode
getVarName (SetTimeDiff _) = TimeDiff
getVarName (SetTimeout _) = Timeout
getVarName (SetEncoding _) = Encoding
getVarName (SetColor _) = Color
getVarName (SetPrompt _) = PromptVar


parseBool :: Text -> Either String Bool
parseBool val = case toLower val of
    "y"     -> return True
    "n"     -> return False
    "yes"   -> return True
    "no"    -> return False
    "true"  -> return True
    "false" -> return False
    "1"     -> return True
    "0"     -> return False
    "on"    -> return True
    "off"   -> return False
    _       -> throwError "Value should be: yes or no"


parseSetVar :: EvalVar -> Text -> Either String VarValue
parseSetVar Mode val = SetMode <$> parseRconMode (unpack val)
parseSetVar TimeDiff val = case decimal val of
    Right (num, "") -> return $ SetTimeDiff num
    _               -> throwError "Value shoud be decimal"

parseSetVar Timeout val = case rational val of
    Right (num, "") -> return $ SetTimeout num
    _               -> throwError "Value should be float"
parseSetVar Encoding val = SetEncoding <$> parseEncoding (unpack val)
parseSetVar Color val = SetColor <$> parseBool val
parseSetVar PromptVar val = SetPrompt <$> readPrompt (unpack val)


disambiguate :: Text -> Maybe Text
disambiguate cmd
    | T.length cmd > 1 = listToMaybe search_cmds
    | otherwise = Nothing
  where
    search_cmds = filter (isPrefixOf cmd) topLevelCommands


firstMap :: (a -> a) -> [a] -> [a]
firstMap _ [] = []
firstMap fun (x:xs) = fun x : xs


internalAutoComplete :: Text -> Text -> [Text]
internalAutoComplete prev cmd = case prev_cmds of
    []       -> searchComp topLevelCommands
    [":set"] -> searchComp setVars
    [":set", "color"]    -> searchComp bools
    [":set", "encoding"] -> searchComp encodings
    _        -> []
  where
    sprev = T.reverse $ strip prev
    prev_cmds = firstMap firstCmd $ T.words sprev
    firstCmd cmd = fromMaybe cmd $ disambiguate cmd
    vars :: [EvalVar]
    vars = enumFromTo minBound maxBound
    setVars = map (pack . show) vars
    searchComp = filter (isPrefixOf cmd)
    bools = map (pack . showBool) [True, False]
    encodings = map (pack . showEncoding) $ enumFromTo minBound maxBound


parseCommand :: String -> EvalCmd
parseCommand command
    | tcommand == empty = return Empty
    | isPrefixOf ":" tcommand = parseInternalCommand dcmd (strip args)
    | otherwise = return $ RconCommand tcommand
  where
    tcommand = strip $ pack command
    (cmd, args) = break isSpace tcommand
    dcmd = fromMaybe cmd $ disambiguate cmd


parseInternalCommand :: Text -> Text -> EvalCmd
parseInternalCommand ":quit" _ = return Quit
parseInternalCommand ":help" "" = return Help
parseInternalCommand ":?" "" = return Help
parseInternalCommand ":" "" = return RepeatLast
parseInternalCommand ":login" "" = return Login
parseInternalCommand ":version" "" = return Version
parseInternalCommand ":history" "" = return $ History Nothing
parseInternalCommand ":history" v = case decimal v of
    (Right (num, "")) -> return $ History (Just num)
    _                 -> throwError $ WrongArgumentType ":history" v

parseInternalCommand ":set" "" = return ListVars
parseInternalCommand ":set" v = case margs of
    Just (var, "")  -> return $ Show var
    Just (var, arg) -> either toErr return (Set <$> parseSetVar var arg)
    Nothing         -> throwError $ WrongArgumentType ":set" v
  where
    (var_str, var_arg) = break isSpace v
    margs = (\v -> (v, strip var_arg)) <$> parseEvalVar var_str
    toErr = throwError . OtherError . pack

parseInternalCommand cmd args
    | cmd `elem` withoutArgs = throwError $ CommandTakesNoArgumens cmd args
    | otherwise = throwError $ UnknownCommand cmd
  where
    withoutArgs = [":", ":help", ":?", ":version", ":login"]
