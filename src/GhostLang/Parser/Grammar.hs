module GhostLang.Parser.Grammar
    ( ghostModuleDef
    , moduleDecl
    , importDecl
    , valueRef
    ) where

import GhostLang.Intrinsic (IntrinsicSet)
import GhostLang.Types ( GhostModule (..)
                       , ModuleDecl (..)
                       , ImportDecl (..)
                       , Value (..)
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

-- | Parse a value reference. I.e. everywhere a value is used.
valueRef :: Parser Value
valueRef = literalRef <|> gaussianRef <|> uniformRef <|> storedRef
    where
      literalRef = Literal <$> (reserved "literal" *> withinParens nonNegative)
      gaussianRef = do
        (v1, v2) <- reserved "gaussian" *> withinParens valuePair
        return $ Gaussian v1 v2
      uniformRef = do
        (v1, v2) <- reserved "uniform" *> withinParens valuePair
        return $ Uniform v1 v2
      storedRef  = Stored . T.pack <$> identifier
      valuePair = do
        v1 <- nonNegative
        comma
        v2 <- nonNegative
        return (v1, v2)
