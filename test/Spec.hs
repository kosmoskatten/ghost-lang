module Main
    ( main
    ) where

import GhostLang.InterpreterProps ( simpleSequencePattern
                                  , manySimpleSequencePatterns
                                  , nonNestedLoopPattern
                                  , nonNestedConcPattern
                                  )
import GhostLang.InterpreterTests ( oneLevelCallNoParamsPattern
                                  , oneLevelCallOneParamPattern
                                  , localScopeOneParamPattern
                                  , twoLevelTwoParamsPattern
                                  , longChainTwoParamsPattern
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
    , testGroup "GhostLang.InterpreterTests"
      [ testCase "OneLevelCallNoParamsPattern" oneLevelCallNoParamsPattern
      , testCase "OneLevelCallOneParamPattern" oneLevelCallOneParamPattern
      , testCase "LocalScopeOneParamPattern" localScopeOneParamPattern
      , testCase "TwoLevelTwoParamsPattern" twoLevelTwoParamsPattern
      , testCase "LongChainTwoParamsPattern" longChainTwoParamsPattern
      ]
    , testGroup "GhostLang.SerializationTests"
      [ testProperty "EncodeDecodeIsEqual" encodeDecodeIsEqual
      , testCase "WriteReadFile" writeReadFile
      ]
    ]
