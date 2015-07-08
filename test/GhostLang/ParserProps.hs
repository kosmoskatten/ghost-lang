module GhostLang.ParserProps 
    ( moduleDeclaration
    ) where

import GhostLang.Types (ModuleDecl (..))
import GhostLang.Parser.Grammar (moduleDecl)
import GhostLang.ParserGenerators
import Text.Parsec (parse)

-- | Property to test the moduleDecl parser.
moduleDeclaration :: ModuleDecl -> Bool
moduleDeclaration m =
    case parse moduleDecl "" (stringify m) of
      Right m' -> m == m'
      _        -> False
