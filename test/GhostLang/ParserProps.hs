module GhostLang.ParserProps 
    ( ghostModuleDefinition
    , moduleDeclaration
    , importDeclaration
    , valueReference
    ) where

import GhostLang.Intrinsic (IntrinsicSet)
import GhostLang.Types ( GhostModule (..)
                       , ModuleDecl (..)
                       , ImportDecl (..)
                       , Value (..)
                       )
import GhostLang.Parser.Grammar ( ghostModuleDef
                                , moduleDecl
                                , importDecl
                                , valueRef
                                )
import GhostLang.ParserGenerators
import Text.Parsec (parse)

-- | Property to test the top level ghostModuleDef parser,
ghostModuleDefinition :: GhostModule IntrinsicSet -> Bool
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

-- | Property to test the importDecl parser.
importDeclaration :: ImportDecl -> Bool
importDeclaration i =
    case parse importDecl "" (stringify i) of
      Right i' -> i == i'
      _        -> False

-- | Property to test the valuRef parser.
valueReference :: Value -> Bool
valueReference v =
    case parse valueRef "" (stringify v) of
      Right v' -> v == v'
      _        -> False
