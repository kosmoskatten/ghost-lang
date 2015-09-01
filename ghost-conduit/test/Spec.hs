module Main 
    ( main
    ) where

import GhostLang.ConduitProps (sourceAndSinkShallMatch)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
    [ testGroup "GhostLang.ConduitProps"
      [ testProperty "SourceAndSinkShallMatch" sourceAndSinkShallMatch
      ]
    ]
