module Main
    ( main
    ) where

import GhostLang.FlowTests ( initStateTest
                           , setHttpConfigTest
                           , loadNonExistingProgramTest
                           , loadDefunctProgramTest
                           , loadCompilableProgramTest
                           )
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite = 
    [ testGroup "GhostLang.FlowTests"
      [ testCase "InitialState" initStateTest
      , testCase "SetHttpConfig" setHttpConfigTest
      , testCase "LoadNonexistingProgram" loadNonExistingProgramTest
      , testCase "LoadDefunctProgram" loadDefunctProgramTest
      , testCase "LoadCompilableProgram" loadCompilableProgramTest
      ]
    ]

