{-# LANGUAGE TupleSections #-}
module GhostLang.Parser.Grammar
    ( ghostModuleDef
    , moduleDecl
    , importDecl
    , valueRef
    , timeUnitRef
    , intrinsicCommand
    , pattern
    , procedure
    , operation
    ) where

import GhostLang.Intrinsic (IntrinsicSet (..))
import GhostLang.Types ( Label
                       , GhostModule (..)
                       , ModuleDecl (..)
                       , ImportDecl (..)
                       , Value (..)
                       , TimeUnit (..)
                       , Pattern (..)
                       , Procedure (..)
                       , Operation (..)
                       )
import GhostLang.Parser.Tokenizer ( comma
                                  , identifier
                                  , nonNegative
                                  , moduleSegment
                                  , reserved
                                  , whiteSpace
                                  , withinBraces
                                  , withinParens )
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Data.Text as T

-- | Parse a ghost module definition. I.e. the main structure for a
-- ghost-lang module.
ghostModuleDef :: Parser (GhostModule IntrinsicSet)
ghostModuleDef =
  GhostModule <$> (whiteSpace *> moduleDecl)
              <*> many importDecl 
              <*> many pattern
              <*> many procedure

-- | Parse a module declaration. I.e. the keyword "module" followed by a
-- module path.
moduleDecl :: Parser ModuleDecl
moduleDecl =
  ModuleDecl <$> getPosition <* reserved "module" 
             <*> moduleSegment `sepBy1` char '.'

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
      storedRef   = Stored <$> packedIdentifier
      valuePair   = (,) <$> nonNegative <*> (comma *> nonNegative)

-- | Parse a time reference from the stream
timeUnitRef :: Parser TimeUnit
timeUnitRef = do
  value <- valueRef
  unit  <- reserved "usec" *> pure USec
       <|> reserved "msec" *> pure MSec
       <|> reserved "sec"  *> pure Sec
  return $ unit value

-- | Parse an intrinsic command from the stream.
intrinsicCommand :: Parser IntrinsicSet
intrinsicCommand = Delay <$> (reserved "Delay" *> timeUnitRef)

-- | Parse a pattern from the stream.
pattern :: Parser (Pattern IntrinsicSet)
pattern =
  Pattern <$> getPosition <* reserved "pattern"
          <*> packedIdentifier 
          <*> (reserved "with" >> reserved "weight" *> nonNegative)
          <*> withinBraces opsList

-- | Parse a procedure from the stream.
procedure :: Parser (Procedure IntrinsicSet)
procedure = do
  reserved "procedure"
  Procedure <$> packedIdentifier
            <*> withinParens idList
            <*> withinBraces opsList

-- | Parse an operation from the stream.
operation :: Parser (Operation IntrinsicSet)
operation = concurrently <|> loop <|> invoke <|> unresolved
    where
      concurrently = do
        reserved "concurrently"
        Concurrently <$> withinBraces opsList               
      loop         = do
        reserved "loop"
        Loop <$> valueRef <*> withinBraces opsList
      invoke       = Invoke <$> intrinsicCommand
      unresolved   = Unresolved <$> getPosition
                                <*> packedIdentifier 
                                <*> (withinParens $ valueRef `sepBy` comma)

idList :: Parser [Label]
idList = packedIdentifier `sepBy` comma

opsList :: Parser [Operation IntrinsicSet]
opsList = operation `sepBy` comma

packedIdentifier :: Parser Label
packedIdentifier = T.pack <$> identifier

