module Main
    ( main
    ) where

import GhostLang.InterpreterTests ( simpleSequencePattern
                                  , manySimpleSequencePatterns
                                  , nonNestedLoopPattern
                                  , nonNestedConcPattern
                                  , oneLevelCallNoParamsPattern
                                  , oneLevelCallOneParamPattern
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
    [ testGroup "GhostLang.InterpreterTests"
      [ testProperty "SimpleSequencePattern" simpleSequencePattern
      , testProperty "ManySimpleSequencePatterns" manySimpleSequencePatterns
      , testProperty "NonNestedLoopPattern" nonNestedLoopPattern
      , testProperty "NonNestedConcPattern" nonNestedConcPattern
      , testCase "OneLevelCallNoParamsPattern" oneLevelCallNoParamsPattern
      , testCase "OneLevelCallOneParamPattern" oneLevelCallOneParamPattern
      ]
    , testGroup "GhostLang.SerializationTests"
      [ testProperty "EncodeDecodeIsEqual" encodeDecodeIsEqual
      , testCase "WriteReadFile" writeReadFile
      ]
    ]
