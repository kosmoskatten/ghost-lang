module Main
    ( main
    ) where

import GhostLang.InterpreterTests (simpleInvokeTest)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
    [ testGroup "GhostLang.InterpreterTests"
      [ testCase "Simple Invoke Test" simpleInvokeTest
      ]
    ]
