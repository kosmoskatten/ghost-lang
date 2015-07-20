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
import GhostLang.ParserProps ( ghostModuleDefinition
                             , moduleDeclaration
                             , importDeclaration
                             , valueReference
                             , timeUnitReference
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
    , testGroup "GhostLang.ParserProps"
      [ testProperty "GhostModuleDefinition" ghostModuleDefinition
      , testProperty "ModuleDeclaration" moduleDeclaration
      , testProperty "ImportDeclaration" importDeclaration
      , testProperty "ValueReference" valueReference
      , testProperty "TimeUnitReference" timeUnitReference
      ]
    , testGroup "GhostLang.SerializationTests"
      [ testProperty "EncodeDecodeIsEqual" encodeDecodeIsEqual
      , testCase "WriteReadFile" writeReadFile
      ]
    ]
