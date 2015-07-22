module GhostLang.ParserProps 
    ( ghostModuleDefP
    , moduleDeclP
    , importDeclP
    , valueRefP
    , timeUnitRefP
    , intrinsicCommandP
    , operationP
    ) where

import GhostLang.Intrinsic (IntrinsicSet)
import GhostLang.Types ( GhostModule
                       , ModuleDecl
                       , ImportDecl
                       , Value
                       , TimeUnit
                       , Operation
                       )
import GhostLang.Parser.Grammar ( ghostModuleDef
                                , moduleDecl
                                , importDecl
                                , valueRef
                                , timeUnitRef
                                , intrinsicCommand
                                , operation
                                )
import GhostLang.CommonGenerators
import GhostLang.ParserGenerators
import Text.Parsec (parse)

-- | Property to test the top level ghostModuleDef parser,
ghostModuleDefP :: GhostModule IntrinsicSet -> Bool
ghostModuleDefP g =
    case parse ghostModuleDef "" (stringify g) of
      Right g' -> g == g'
      _        -> False

-- | Property to test the moduleDecl parser.
moduleDeclP :: ModuleDecl -> Bool
moduleDeclP m =
    case parse moduleDecl "" (stringify m) of
      Right m' -> m == m'
      _        -> False

-- | Property to test the importDecl parser.
importDeclP :: ImportDecl -> Bool
importDeclP i =
    case parse importDecl "" (stringify i) of
      Right i' -> i == i'
      _        -> False

-- | Property to test the valueRef parser.
valueRefP :: Value -> Bool
valueRefP v =
    case parse valueRef "" (stringify v) of
      Right v' -> v == v'
      _        -> False

-- | Property to test the timeUnitRef parser.
timeUnitRefP :: TimeUnit -> Bool
timeUnitRefP t =
    case parse timeUnitRef "" (stringify t) of
      Right t' -> t == t'
      _        -> False

-- | Property to test the instrinsicCommand parser.
intrinsicCommandP :: IntrinsicSet -> Bool
intrinsicCommandP i =
    case parse intrinsicCommand "" (stringify i) of
      Right i' -> i == i'
      _        -> False

-- | Property to test the operation parser.
operationP :: Operation IntrinsicSet -> Bool
operationP o =
    case parse operation "" (stringify o) of
      Right o' -> o == o'
      _        -> False
