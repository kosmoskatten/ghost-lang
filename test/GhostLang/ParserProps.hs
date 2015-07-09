module GhostLang.ParserProps 
    ( ghostModuleDefinition
    , moduleDeclaration
    , importDeclaration
    ) where

import GhostLang.Types ( GhostModule (..)
                       , ModuleDecl (..)
                       , ImportDecl (..)
                       )
import GhostLang.Parser.Grammar ( ghostModuleDef
                                , moduleDecl
                                , importDecl
                                )
import GhostLang.ParserGenerators
import Text.Parsec (parse)

-- | Property to test the top level ghostModuleDef parser,
ghostModuleDefinition :: GhostModule -> Bool
ghostModuleDefinition g =
    case parse ghostModuleDef "" (stringify g) of
      Right g' -> g == g'
      _        -> False

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
