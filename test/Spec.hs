module Main
    ( main
    ) where

import GhostLang.InterpreterTests ( simpleSequencePattern
                                  , manySimpleSequencePatterns
                                  )
import GhostLang.SerializationTests (encodeDecodeIsEqual)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
    [ testGroup "GhostLang.InterpreterTests"
      [ testProperty "SimpleSequencePattern" simpleSequencePattern
      , testProperty "ManySimpleSequencePatterns" manySimpleSequencePatterns
      ]
    , testGroup "GhostLang.SerializationTests"
      [ testProperty "EncodeDecodeIsEqual" encodeDecodeIsEqual
      ]
    ]
