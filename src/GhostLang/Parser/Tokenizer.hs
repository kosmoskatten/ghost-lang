module GhostLang.Parser.Tokenizer 
    ( moduleSegment
    , reserved
    ) where

import Control.Monad (when)
import Data.Char (isLower)
import GhostLang.Types (ModuleSegment)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Data.Text as Text
import qualified Text.Parsec.Token as Token

tokenizer :: Token.TokenParser ()
tokenizer =
    Token.makeTokenParser emptyDef
    { Token.commentLine   = "//"
    , Token.commentStart  = "/*"
    , Token.commentEnd    = "*/"
    , Token.identStart    = letter
    , Token.identLetter   = alphaNum <|> oneOf "-_"
    , Token.reservedNames =
        [ "module"
        ]
    , Token.caseSensitive = True
    }

moduleSegment :: Parser ModuleSegment
moduleSegment = do
  str <- identifier
  when (isLower $ head str) $ 
       parserFail "Module part must start with capital letter"
  return $ Text.pack str

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme tokenizer

reserved :: String -> Parser ()
reserved = lexeme . Token.reserved tokenizer

identifier :: Parser String
identifier = lexeme $ Token.identifier tokenizer
