module GhostLang.ParserProps 
    ( moduleDeclaration
    , importDeclaration
    ) where

import GhostLang.Types (ModuleDecl (..), ImportDecl (..))
import GhostLang.Parser.Grammar (moduleDecl, importDecl)
import GhostLang.ParserGenerators
import Text.Parsec (parse)

-- | Property to test the moduleDecl parser.
moduleDeclaration :: ModuleDecl -> Bool
moduleDeclaration m =
    case parse moduleDecl "" (stringify m) of
      Right m' -> m == m'
      _        -> False

-- | Property to the importDecl parser.
importDeclaration :: ImportDecl -> Bool
importDeclaration i =
    case parse importDecl "" (stringify i) of
      Right i' -> i == i'
      _        -> False
