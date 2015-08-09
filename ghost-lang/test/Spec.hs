module Main
    ( main
    ) where

import GhostLang.InterpreterProps ( simpleSequencePattern
                                  , manySimpleSequencePatterns
                                  , nonNestedLoopPattern
                                  , nonNestedConcPattern
                                  )
import GhostLang.InterpreterMProps (evalValueP, evalTimeUnitP)
import GhostLang.InterpreterTests ( oneLevelCallNoParamsPattern
                                  , oneLevelCallOneParamPattern
                                  , localScopeOneParamPattern
                                  , twoLevelTwoParamsPattern
                                  , longChainTwoParamsPattern
                                  )
import GhostLang.IntrinsicTests (delayCommand)
import GhostLang.LinkerTests ( checkEmptyModule
                             , checkOneNonMainModule
                             , checkMainModuleWithoutPatterns
                             , checkOtherModuleWithPatterns
                             , checkDuplicateMainModules
                             , checkSingleCorrectMainModule
                             , checkTwoCorrectModules
                             , findUndefinedProc
                             , findDefinedProc
                             , findDoubleDefinedProc
                             , resolveLocalProc
                             , resolveImportedProc
                             , resolveProcInProc
                             , resolveInLoopProc
                             , resolveInConcProc
                             , resolveUnimportedProc
                             , resolveAmbiguousProc
                             , resolveConflictingArityProc
                             )
import GhostLang.ParserProps ( ghostModuleDefP
                             , moduleDeclP
                             , importDeclP
                             , valueRefP
                             , timeUnitRefP
                             , payloadRefP
                             , paceRefP
                             , methodP
                             , contentP
                             , intrinsicCommandP
                             , patternP
                             , procedureP
                             , operationP
                             )
import GhostLang.SerializationTests ( encodeDecodeIsEqual
                                    , writeReadFile
                                    )
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
    [ testGroup "GhostLang.InterpreterProps"
      [ testProperty "SimpleSequencePattern" simpleSequencePattern
      , testProperty "ManySimpleSequencePatterns" manySimpleSequencePatterns
      , testProperty "NonNestedLoopPattern" nonNestedLoopPattern
      , testProperty "NonNestedConcPattern" nonNestedConcPattern
      ]
    , testGroup "GhostLang.InterpreterMTests"
      [ testProperty "EvalValue" evalValueP
      , testProperty "EvalTimeUnit" evalTimeUnitP
      ]
    , testGroup "GhostLang.InterpreterTests"
      [ testCase "OneLevelCallNoParamsPattern" oneLevelCallNoParamsPattern
      , testCase "OneLevelCallOneParamPattern" oneLevelCallOneParamPattern
      , testCase "LocalScopeOneParamPattern" localScopeOneParamPattern
      , testCase "TwoLevelTwoParamsPattern" twoLevelTwoParamsPattern
      , testCase "LongChainTwoParamsPattern" longChainTwoParamsPattern
      ]
    , testGroup "GhostLang.IntrinsicTests"
      [ testCase "DelayCommand" delayCommand
      ]
    , testGroup "GhostLang.LinkerTests - prelink checks"
      [ testCase "CheckEmptyModule" checkEmptyModule
      , testCase "CheckOneNonMainModule" checkOneNonMainModule
      , testCase "CheckMainModuleWithoutPatterns" checkMainModuleWithoutPatterns
      , testCase "CheckOtherModuleWithPatterns" checkOtherModuleWithPatterns
      , testCase "CheckDuplicateMainModules" checkDuplicateMainModules
      , testCase "CheckSingleCorrectMainModule" checkSingleCorrectMainModule
      , testCase "CheckTwoCorrectModules" checkTwoCorrectModules
      ]
    , testGroup "GhostLang.LinkerTests - proc lookup"
      [ testCase "FindUndefinedProc" findUndefinedProc
      , testCase "FindDefinedProc" findDefinedProc
      , testCase "FindDoubleDefinedProc" findDoubleDefinedProc
      ]
    , testGroup "GhostLang.LinkerTests - resolving"
      [ testCase "ResolveLocalProc" resolveLocalProc
      , testCase "ResolveImportedProc" resolveImportedProc
      , testCase "ResolveProcInProc" resolveProcInProc
      , testCase "ResolveInLoopProc" resolveInLoopProc
      , testCase "ResolveInConcProc" resolveInConcProc
      , testCase "ResolveUnimportedProc" resolveUnimportedProc
      , testCase "ResolveAmbiguousProc" resolveAmbiguousProc
      , testCase "ResolveConflictingArityProc" resolveConflictingArityProc
      ]
    , testGroup "GhostLang.ParserProps"
      [ testProperty "GhostModuleDefinition" ghostModuleDefP
      , testProperty "ModuleDeclaration" moduleDeclP
      , testProperty "ImportDeclaration" importDeclP
      , testProperty "ValueReference" valueRefP
      , testProperty "TimeUnitReference" timeUnitRefP
      , testProperty "PayloadReference" payloadRefP
      , testProperty "PaceRefP" paceRefP
      , testProperty "Content" contentP
      , testProperty "Method" methodP
      , testProperty "IntrinsicCommand" intrinsicCommandP
      , testProperty "Pattern" patternP
      , testProperty "Procedure" procedureP
      , testProperty "Operation" operationP
      ]
    , testGroup "GhostLang.SerializationTests"
      [ testProperty "EncodeDecodeIsEqual" encodeDecodeIsEqual
      , testCase "WriteReadFile" writeReadFile
      ]
    ]
