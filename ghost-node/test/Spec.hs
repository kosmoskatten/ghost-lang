module Main
    ( main
    ) where

import GhostLang.FlowTests (setHttpConfigTest)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite = 
    [ testGroup "GhostLang.FlowTests"
      [ testCase "SetHttpConfig" setHttpConfigTest
      ]
    ]

