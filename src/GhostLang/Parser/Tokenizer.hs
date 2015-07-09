module GhostLang.Parser.Tokenizer 
    ( moduleSegment
    , reserved
    , whiteSpace
    ) where

import Control.Monad (when)
import Data.Char (isLower)
import GhostLang.Types (ModuleSegment)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Data.Text as Text
import qualified Text.Parsec.Token as Token

-- | Parse one module segment from the stream.
moduleSegment :: Parser ModuleSegment
moduleSegment = do
  str <- identifier
  when (isLower $ head str) $ 
       parserFail "Module part must start with capital letter"
  return $ Text.pack str

-- | Try to parse the given string as a reserved work from the stream.
reserved :: String -> Parser ()
reserved = lexeme . Token.reserved tokenizer

-- | Consume whitespace from the stream.
whiteSpace :: Parser ()
whiteSpace = lexeme $ Token.whiteSpace tokenizer

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
        , "import"
        ]
    , Token.caseSensitive = True
    }

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme tokenizer

identifier :: Parser String
identifier = lexeme $ Token.identifier tokenizer
