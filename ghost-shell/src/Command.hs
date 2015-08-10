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
    LoadProgram   :: !FilePath -> Command
    Status        :: Command
    ListInfo      :: Command
    SetHttpParams :: !String -> !Int -> Command
    RunPattern    :: !String -> !Mode -> Command
    Help          :: Command
    Quit          :: Command
    EmptyLine     :: Command
    Unknown       :: !String -> Command
    deriving Show

-- | Entry point for the parser.
parseCommand :: String -> Command
parseCommand str =
    case runParser aCommand () "" str of
      Right c  -> c
      Left err -> Unknown $ show err

aCommand :: Parser Command
aCommand = spaces *> ( try loadProgram
                   <|> try status
                   <|> try listInfo
                   <|> try setHttpParams
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

status :: Parser Command
status = string "status" >> spaces >> eof >> pure Status

listInfo :: Parser Command
listInfo = string "list-info" >> spaces >> eof >> pure ListInfo

setHttpParams :: Parser Command
setHttpParams = do
  string "set-http-params" >> spaces
  service' <- service
  spaces
  port'    <- port
  spaces >> eof
  return $ SetHttpParams service' port'

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

service :: Parser String
service = (:) <$> oneOf startChar <*> many (oneOf followChar)
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
