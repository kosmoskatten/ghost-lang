{-# LANGUAGE TupleSections #-}
module GhostLang.Parser.Grammar
    ( ghostModuleDef
    , moduleDecl
    , importDecl
    , valueRef
    , timeUnitRef
    ) where

import GhostLang.Intrinsic (IntrinsicSet)
import GhostLang.Types ( GhostModule (..)
                       , ModuleDecl (..)
                       , ImportDecl (..)
                       , Value (..)
                       , TimeUnit (..)
                       )
import GhostLang.Parser.Tokenizer ( comma
                                  , identifier
                                  , nonNegative
                                  , moduleSegment
                                  , reserved
                                  , whiteSpace
                                  , withinParens )
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Data.Text as T

-- | Parse a ghost module definition. I.e. the main structure for a
-- ghost-lang module.
ghostModuleDef :: Parser (GhostModule IntrinsicSet)
ghostModuleDef = do
  whiteSpace
  GhostModule <$> moduleDecl <*> many importDecl <*> pure [] <*> pure []

-- | Parse a module declaration. I.e. the keyword "module" followed by a
-- module path.
moduleDecl :: Parser ModuleDecl
moduleDecl = do
  reserved "module"
  ModuleDecl <$> moduleSegment `sepBy1` char '.'

-- | Parse an import declaration. I.e. the keyword "import" followed
-- by a module path.
importDecl :: Parser ImportDecl
importDecl = do
  reserved "import"
  ImportDecl <$> moduleSegment `sepBy1` char '.'

-- | Parse a value reference from the stream.
valueRef :: Parser Value
valueRef = literalRef <|> gaussianRef <|> uniformRef <|> storedRef
    where
      literalRef  = Literal <$> (reserved "literal" *> withinParens nonNegative)
      gaussianRef = uncurry Gaussian <$> 
                        (reserved "gaussian" *> withinParens valuePair)
      uniformRef  = uncurry Uniform <$> 
                        (reserved "uniform" *> withinParens valuePair)
      storedRef   = Stored . T.pack <$> identifier
      valuePair   = (,) <$> nonNegative <*> (comma *> nonNegative)

-- | Parse a time reference from the stream
timeUnitRef :: Parser TimeUnit
timeUnitRef = do
  value <- valueRef
  unit  <- reserved "usec" *> pure USec
       <|> reserved "msec" *> pure MSec
       <|> reserved "sec"  *> pure Sec
  return $ unit value
