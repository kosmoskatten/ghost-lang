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
    Load :: !FilePath -> Command
    Status :: Command
    List :: Command
    Run :: !String -> !Mode -> Command
    Help :: Command
    Quit :: Command
    EmptyLine :: Command
    Unknown :: !String -> Command
    deriving Show

-- | Entry point for the parser.
parseCommand :: String -> Command
parseCommand str =
    case runParser aCommand () "" str of
      Right c  -> c
      Left err -> Unknown $ show err

aCommand :: Parser Command
aCommand = spaces *> ( try load
                   <|> try status
                   <|> try list
                   <|> try run
                   <|> try help 
                   <|> try quit 
                   <|> emptyLine )

load :: Parser Command
load = do
  string "load-program" >> spaces
  path' <- path
  spaces >> eof
  return $ Load path'

status :: Parser Command
status = string "status" >> spaces >> eof >> pure Status

list :: Parser Command
list = string "list-patterns" >> spaces >> eof >> pure List

run :: Parser Command
run = do
  string "run-pattern" >> spaces
  pattern' <- pattern
  spaces
  mode' <- mode
  spaces >> eof
  return $ Run pattern' mode'

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

mode :: Parser Mode
mode = string "mode" >> spaces >> char '=' >> spaces >> mode'
    where mode' = string "normal" *> pure Normal
              <|> string "trace"  *> pure Trace
              <|> string "dry"    *> pure Dry
