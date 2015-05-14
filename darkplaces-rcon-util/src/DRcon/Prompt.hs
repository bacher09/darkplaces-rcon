module DRcon.Prompt (
    Prompt,
    parsePrompt
) where
import qualified Data.Map.Strict as SM
import Data.Tuple (swap)


data FormaterToken a = SimpleText a
                     | ServerName
                     | ServerHost
                     | ServerPort
                     | SystemTime
                     | SystemTimeSeconds
                     | SystemDate
                     | ConnectMode
    deriving(Show, Read, Eq, Ord)


type PromptToken = FormaterToken String
type PromptFormater = [PromptToken]
type TokenRender = (PromptToken -> String)
newtype Prompt = Prompt PromptFormater


instance Show Prompt where
    show (Prompt ts) = show $ concatEscape $ map renderToken ts


formatSymbols :: [(Char, PromptToken)]
formatSymbols = [
    ('N', ServerName),
    ('h', ServerHost),
    ('p', ServerPort),
    ('T', SystemTime),
    ('*', SystemTimeSeconds),
    ('D', SystemDate),
    ('m', ConnectMode),
    ('%', SimpleText "%")]


formatSymbolsMap :: SM.Map Char PromptToken
formatSymbolsMap = SM.fromList formatSymbols


simpleParser :: String -> PromptFormater
simpleParser ('%':x:xs) = case maybeToken of
    Just t -> t : simpleParser xs
    Nothing -> case simpleParser (x:xs) of
        ((SimpleText str):ts) -> SimpleText ('%':str) : ts
        ts                    -> (SimpleText "%") : ts
  where
    maybeToken = SM.lookup x formatSymbolsMap

simpleParser (x:xs) = case simpleParser xs of
    ((SimpleText str):ts) -> SimpleText (x:str) : ts
    ts                    -> SimpleText (x:[])  : ts
simpleParser "" = []


escapeChars :: String -> String
escapeChars ('%':x:xs) = case SM.lookup x formatSymbolsMap of
    Just _  -> '%':'%' : escapeChars (x:xs)
    Nothing -> '%' : escapeChars (x:xs)
escapeChars (x:xs) = x : escapeChars xs
escapeChars "" = ""


appendEscape :: String -> String -> String
appendEscape "" s = s
appendEscape f "" = f
appendEscape f s
    | last_f == '%' = case SM.lookup head_s formatSymbolsMap of
        Just _  -> f ++ "%" ++ s
        Nothing -> f ++ s
    | otherwise = f ++ s
  where
    last_f = last f
    head_s = head s


concatEscape :: [String] -> String
concatEscape = foldr appendEscape ""


renderToken :: TokenRender
renderToken (SimpleText a) = escapeChars a
renderToken t = case SM.lookup t tokensMap of
    Just c  -> '%':c:[]
    Nothing -> ""
  where
    tokensMap = SM.fromList $ map swap formatSymbols


renderPromp :: TokenRender -> Prompt -> String
renderPromp fun (Prompt prompt) = concatMap fun prompt


parsePrompt :: String -> Prompt
parsePrompt = Prompt . simpleParser
