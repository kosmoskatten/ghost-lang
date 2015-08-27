module Main
    ( main
    ) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite = []
