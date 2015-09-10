{-# LANGUAGE GADTs #-}
module Command 
    ( Command (..)
    , parseCommand
    ) where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)

-- | Simple data type to model the shell commands.
data Command where
    LoadProgram         :: !FilePath -> Command
    ListSelectedProgram :: Command
    ListPrograms        :: Command
    ListPatterns        :: Command
    GetHttpConfig       :: Command
    SetHttpConfig       :: !String -> !Int -> Command
    RunNamedPattern     :: !String -> !Bool -> !(Maybe String) -> Command
    RunRandomPattern    :: !Bool -> !(Maybe String) -> Command
    ListGlobalCounter   :: Command
    Help                :: Command
    Quit                :: Command
    EmptyLine           :: Command
    Unknown             :: !String -> Command
    deriving Show

-- | Entry point for the parser.
parseCommand :: String -> Command
parseCommand str =
    case runParser aCommand () "" str of
      Right c  -> c
      Left err -> Unknown $ show err

aCommand :: Parser Command
aCommand = spaces *> ( try loadProgram
                   <|> try listSelectedProgram
                   <|> try listPrograms
                   <|> try listPatterns
                   <|> try getHttpConfig
                   <|> try setHttpConfig
                   <|> try runNamedPattern
                   <|> try runRandomPattern
                   <|> try listGlobalCounter
                   <|> try help 
                   <|> try quit 
                   <|> emptyLine )

loadProgram :: Parser Command
loadProgram = do
  keyword "load-program"
  LoadProgram <$> ((spaces *> path) <* (spaces >> eof))

listSelectedProgram :: Parser Command
listSelectedProgram = 
    keyword "list-selected-program" >> spaces >> eof >> pure ListSelectedProgram

listPrograms :: Parser Command
listPrograms = keyword "list-programs" >> spaces >> eof >> pure ListPrograms

listPatterns :: Parser Command
listPatterns = keyword "list-patterns" >> spaces >> eof >> pure ListPatterns

getHttpConfig :: Parser Command
getHttpConfig = keyword "get-http-config" >> spaces >> eof >> pure GetHttpConfig

setHttpConfig :: Parser Command
setHttpConfig = do
  keyword "set-http-config"
  SetHttpConfig <$> (spaces *> server)
                <*> ((spaces *> port) <* (spaces >> eof))

runNamedPattern :: Parser Command
runNamedPattern = do
  keyword "run-named-pattern"
  RunNamedPattern <$> (spaces *> pattern)
                  <*> (spaces *> trace)
                  <*> ((spaces *> maybeSrcIp) <* (spaces >> eof))

runRandomPattern :: Parser Command
runRandomPattern = do
  keyword "run-random-pattern"
  RunRandomPattern <$> (spaces *> trace)
                   <*> ((spaces *> maybeSrcIp) <* (spaces >> eof))

listGlobalCounter :: Parser Command
listGlobalCounter = keyword "list-global-counter" >> spaces >> eof 
                            >> pure ListGlobalCounter

help :: Parser Command
help = string "help" >> spaces >> eof >> pure Help

quit :: Parser Command
quit = string "quit" >> spaces >> eof >> pure Quit

emptyLine :: Parser Command
emptyLine = eof *> pure EmptyLine

pattern :: Parser String
pattern = (:) <$> oneOf ['a'..'z'] <*> many (oneOf patternChar)
    where patternChar = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "._-"

path :: Parser FilePath
path = (:) <$> char '/' <*> many1 (oneOf pathChar)
    where pathChar = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "._-/"

server :: Parser String
server = (:) <$> oneOf startChar <*> many (oneOf followChar)
    where
      startChar  = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
      followChar = startChar ++ ".-:/"

port :: Parser Int
port = read <$> many1 digit
  
trace :: Parser Bool
trace = string "trace" >> spaces >> string "=" >> spaces >> bool

bool :: Parser Bool
bool = string "true" *> pure True <|> string "false" *> pure False

maybeSrcIp :: Parser (Maybe String)
maybeSrcIp = try (Just <$> srcIp) <|> pure Nothing

srcIp :: Parser String
srcIp = string "src" >> spaces >> string "=" >> spaces >> ipAddress

ipAddress :: Parser String
ipAddress = concat <$> sequence [ ipSeg, string "."
                                , ipSeg, string "."
                                , ipSeg, string "."
                                , ipSeg ] 

ipSeg :: Parser String
ipSeg = do
  seg <- many1 digit
  let segInt = read seg :: Int
  if (segInt >= 0 && segInt < 256) then return seg
  else parserFail "IP segment must be [0 - 255]"

keyword :: String -> Parser ()
keyword = void . string
