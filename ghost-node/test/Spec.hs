module Main
    ( main
    ) where

import GhostLang.FlowTests ( initStateTest
                           , setHttpConfigTest
                           , loadNonExistingProgramTest
                           , loadDefunctProgramTest
                           , loadCompilableProgramTest
                           , runPatternFromNonExistingProgramTest
                           , runNonExistingPatternTest
                           , runExistingPatternTest
                           , patternStatusNonExistingPatternTest
                           , patternStatusFailingPatternTest
                           , patternStatusCompletedPatternTest
                           , patternStatusStillRunningTest
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
      , testCase "RunPatternFromNonExistingProgram" 
                 runPatternFromNonExistingProgramTest
      , testCase "RunNonExistingPattern" runNonExistingPatternTest
      , testCase "RunExistingPattern" runExistingPatternTest
      , testCase "PatternStatusNonExistingPattern"
                 patternStatusNonExistingPatternTest
      , testCase "PatternStatusFailingPattern"
                 patternStatusFailingPatternTest
      , testCase "PatternStatusCompletedPatternTest"
                 patternStatusCompletedPatternTest
      , testCase "PatternStatusStillRunningTest"
                 patternStatusStillRunningTest
      ]
    ]

