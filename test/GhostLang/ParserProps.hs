module GhostLang.ParserProps 
    ( ghostModuleDefP
    , moduleDeclP
    , importDeclP
    , valueRefP
    , timeUnitRefP
    , intrinsicCommandP
    , patternP
    , procedureP
    , operationP
    ) where

import GhostLang.Intrinsic (IntrinsicSet)
import GhostLang.Types ( GhostModule
                       , ModuleDecl
                       , ImportDecl
                       , Value
                       , TimeUnit
                       , Pattern
                       , Procedure
                       , Operation
                       )
import GhostLang.Parser.Grammar ( ghostModuleDef
                                , moduleDecl
                                , importDecl
                                , valueRef
                                , timeUnitRef
                                , intrinsicCommand
                                , pattern
                                , procedure
                                , operation
                                )
import GhostLang.CommonGenerators
import GhostLang.ParserGenerators
import Text.Parsec (parse)
import Text.Parsec.String (Parser)

-- | Property to test the top level ghostModuleDef parser,
ghostModuleDefP :: GhostModule IntrinsicSet -> Bool
ghostModuleDefP = prop ghostModuleDef

-- | Property to test the moduleDecl parser.
moduleDeclP :: ModuleDecl -> Bool
moduleDeclP = prop moduleDecl

-- | Property to test the importDecl parser.
importDeclP :: ImportDecl -> Bool
importDeclP = prop importDecl

-- | Property to test the valueRef parser.
valueRefP :: Value -> Bool
valueRefP = prop valueRef

-- | Property to test the timeUnitRef parser.
timeUnitRefP :: TimeUnit -> Bool
timeUnitRefP = prop timeUnitRef

-- | Property to test the instrinsicCommand parser.
intrinsicCommandP :: IntrinsicSet -> Bool
intrinsicCommandP = prop intrinsicCommand

-- | Property to test the pattern parser.
patternP :: Pattern IntrinsicSet -> Bool
patternP = prop pattern

-- | Property to test the procedure parser.
procedureP :: Procedure IntrinsicSet -> Bool
procedureP = prop procedure

-- | Property to test the operation parser.
operationP :: Operation IntrinsicSet -> Bool
operationP = prop operation

prop :: (Eq a, Stringify a) => Parser a -> a -> Bool
prop p x =
    case parse p "" (stringify x) of
      Right x' -> x == x'
      _        -> False
