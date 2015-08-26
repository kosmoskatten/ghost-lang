{-# LANGUAGE GADTs #-}
module Command 
    ( Command (..)
    , parseCommand
    ) where

import GhostLang (Mode (..))
import Text.Parsec
import Text.Parsec.String (Parser)

-- | Simple data type to model the shell commands.
data Command where
    LoadProgram         :: !FilePath -> Command
    ListSelectedProgram :: Command
    ListPrograms        :: Command
    GetHttpConfig       :: Command
    SetHttpConfig       :: !String -> !Int -> Command
    RunNamedPattern     :: !String -> !Bool -> !(Maybe String) -> Command


    Status              :: Command
    RunPattern          :: !String -> !Mode -> Command
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
                   <|> try getHttpConfig
                   <|> try setHttpConfig
                   <|> try status
                   <|> try runPattern
                   <|> try help 
                   <|> try quit 
                   <|> emptyLine )

loadProgram :: Parser Command
loadProgram = do
  string "load-program" >> spaces
  path' <- path
  spaces >> eof
  return $ LoadProgram path'

listSelectedProgram :: Parser Command
listSelectedProgram = 
    string "list-selected-program" >> spaces >> eof >> pure ListSelectedProgram

listPrograms :: Parser Command
listPrograms = string "list-programs" >> spaces >> eof >> pure ListPrograms

getHttpConfig :: Parser Command
getHttpConfig = string "get-http-config" >> spaces >> eof >> pure GetHttpConfig

setHttpConfig :: Parser Command
setHttpConfig = do
  string "set-http-config" >> spaces
  server' <- server
  spaces
  port' <- port
  spaces >> eof
  return $ SetHttpConfig server' port'


status :: Parser Command
status = string "status" >> spaces >> eof >> pure Status

runPattern :: Parser Command
runPattern = do
  string "run-pattern" >> spaces
  pattern' <- pattern
  spaces
  mode' <- mode
  spaces >> eof
  return $ RunPattern pattern' mode'

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
  
mode :: Parser Mode
mode = string "mode" >> spaces >> char '=' >> spaces >> mode'
    where mode' = string "normal" *> pure Normal
              <|> string "trace"  *> pure Trace
              <|> string "dry"    *> pure Dry

ipAddress :: Parser String
ipAddress = concat <$> sequence [ ipSeg, string "."
                                , ipSeg, string "."
                                , ipSeg, string "."
                                , ipSeg ] 

ipSeg :: Parser String
ipSeg = do
  seg <- many1 $ oneOf ['0'..'9']
  let segInt = read seg :: Int
  if (segInt >= 0 && segInt < 256) then return seg
  else parserFail "IP segment must be [0 - 255]"
