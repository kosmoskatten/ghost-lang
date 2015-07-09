module GhostLang.Parser.Grammar
    ( ghostModuleDef
    , moduleDecl
    , importDecl
    ) where

import GhostLang.Types ( GhostModule (..)
                       , ModuleDecl (..)
                       , ImportDecl (..)
                       )
import GhostLang.Parser.Tokenizer (moduleSegment, reserved)
import Text.Parsec
import Text.Parsec.String (Parser)

-- | Parse a ghost module definition. I.e. the main structure for a
-- ghost-lang module.
ghostModuleDef :: Parser GhostModule
ghostModuleDef = GhostModule <$> moduleDecl <*> many importDecl

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
