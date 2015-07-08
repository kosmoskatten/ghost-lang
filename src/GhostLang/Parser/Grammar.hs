module GhostLang.Parser.Grammar
    ( moduleDecl
    ) where

import GhostLang.Types (ModuleDecl (..))
import GhostLang.Parser.Tokenizer (moduleSegment, reserved)
import Text.Parsec
import Text.Parsec.String (Parser)

-- | Parse a module declaration. I.e. the keyword module followed by a
-- module path.
moduleDecl :: Parser ModuleDecl
moduleDecl = do
  reserved "module"
  ModuleDecl <$> moduleSegment `sepBy1` char '.'
