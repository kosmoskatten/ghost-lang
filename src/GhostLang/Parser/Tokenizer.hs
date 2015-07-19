module GhostLang.Parser.Tokenizer 
    ( comma
    , identifier
    , nonNegative
    , moduleSegment
    , reserved
    , whiteSpace
    , withinParens
    ) where

import Control.Monad (void, when)
import Data.Char (isLower)
import GHC.Int (Int64)
import GhostLang.Types (ModuleSegment)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Printf (printf)
import qualified Data.Text as Text
import qualified Text.Parsec.Token as Token

-- | Parse a comma from the stream.
comma :: Parser ()
comma = void $ lexeme $ Token.comma tokenizer

-- | Parse an identifier from the stream.
identifier :: Parser String
identifier = lexeme $ Token.identifier tokenizer

-- | Parse an Int64 >= 0 from the stream.
nonNegative :: Parser Int64
nonNegative = do
  value <- integer
  let maxLimit = toInteger (maxBound :: Int64)
  when (value < 0 || value > maxLimit) $
       parserFail $ printf "Must be within range [0, %s]" (show maxLimit)
  return $ fromIntegral value

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

-- | Parse from within a pair of parens.
withinParens :: Parser a -> Parser a
withinParens = lexeme . Token.parens tokenizer

integer :: Parser Integer
integer = lexeme $ Token.integer tokenizer

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
        , "literal"
        ]
    , Token.caseSensitive = True
    }

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme tokenizer
