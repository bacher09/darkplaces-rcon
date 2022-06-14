module DRcon.Prompt (
    PromptVars(..),
    Prompt,
    parsePrompt,
    formatPrompt,
    renderPrompt,
    getPromptVars,
    readPrompt,
    defaultPrompt,
    promptEnvName
) where
import qualified Data.Map.Strict as SM
import Data.Tuple (swap)
import Data.Time.LocalTime
import Control.Monad.Except
import Data.Time.Format (formatTime)
import DarkPlaces.Rcon
import DRcon.Polyfills (readMaybe)
import DRcon.Version (versionStr, programName)
import DRcon.Polyfills (defaultTimeLocale)


data FormaterToken a = SimpleText a
                     | ServerName
                     | ServerHost
                     | ServerPort
                     | SystemTime
                     | SystemTimeSeconds
                     | SystemDate
                     | ConnectMode
                     | ProgramName
                     | ProgramVersion
    deriving(Show, Read, Eq, Ord)


data PromptVars = PromptVars {
    promptName        :: String,
    promptHost        :: String,
    promptPort        :: String,
    promptTime        :: String,
    promptTimeSeconds :: String,
    promptDate        :: String,
    promptConnectMode :: String}
    deriving (Show, Read, Eq)


type PromptToken = FormaterToken String
type PromptFormater = [PromptToken]
type TokenRender = (PromptToken -> String)
newtype Prompt = Prompt PromptFormater
    deriving (Eq)


instance Show Prompt where
    show (Prompt ts) = show $ concatEscape $ map renderToken ts


defaultPrompt :: String
defaultPrompt = "%P %N> "


promptEnvName :: String
promptEnvName = "DRCON_PROMPT"


formatSymbols :: [(Char, PromptToken)]
formatSymbols = [
    ('N', ServerName),
    ('h', ServerHost),
    ('p', ServerPort),
    ('T', SystemTime),
    ('*', SystemTimeSeconds),
    ('D', SystemDate),
    ('m', ConnectMode),
    ('P', ProgramName),
    ('v', ProgramVersion),
    ('%', SimpleText "%"),
    ('{', SimpleText "\ESC"), -- same as \ESC
    ('}', SimpleText "\STX")] -- same as \STX


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


tokenRenderFrom :: PromptVars -> TokenRender
tokenRenderFrom _ (SimpleText a) = a
tokenRenderFrom v ServerName = promptName v
tokenRenderFrom v ServerHost = promptHost v
tokenRenderFrom v ServerPort = promptPort v
tokenRenderFrom v SystemTime = promptTime v
tokenRenderFrom v SystemTimeSeconds = promptTimeSeconds v
tokenRenderFrom v SystemDate = promptDate v
tokenRenderFrom v ConnectMode = promptConnectMode v
tokenRenderFrom _ ProgramName = programName
tokenRenderFrom _ ProgramVersion = versionStr


formatPrompt :: TokenRender -> Prompt -> String
formatPrompt fun (Prompt prompt) = concatMap fun prompt


parsePrompt :: String -> Prompt
parsePrompt = Prompt . simpleParser


renderPrompt :: PromptVars -> Prompt -> String
renderPrompt vars prom = formatPrompt (tokenRenderFrom vars) prom


getPromptVars :: String -> RconConnection -> IO PromptVars
getPromptVars name con = do
    date_time <- getZonedTime
    host <- getHost con
    port <- getPort con
    mode <- getMode con
    return PromptVars {promptName=name,
                       promptHost=host,
                       promptPort=port,
                       promptTime=dateFormater date_time "%R",
                       promptTimeSeconds=dateFormater date_time "%T",
                       promptDate=dateFormater date_time "%F",
                       promptConnectMode=show $ fromEnum mode}
  where
    dateFormater t f = formatTime defaultTimeLocale f t


readPrompt :: String -> Either String String
readPrompt "" = return ""
readPrompt arg@('"':_) = case readMaybe arg of
    Just r -> return r
    Nothing -> throwError "Error parsing prompt"

readPrompt arg = readPrompt $ "\"" ++ arg ++ "\""
