{-# LANGUAGE GADTs #-}
module Command 
    ( Command (..)
    , parseCommand
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)

-- | Simple data type to model the shell commands.
data Command where
    Load :: !FilePath -> Command
    Status :: Command
    List :: Command
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

help :: Parser Command
help = string "help" >> spaces >> eof >> pure Help

quit :: Parser Command
quit = string "quit" >> spaces >> eof >> pure Quit

emptyLine :: Parser Command
emptyLine = eof *> pure EmptyLine

path :: Parser FilePath
path = (:) <$> char '/' <*> many1 (oneOf pathChar)
    where pathChar = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "._-/"

