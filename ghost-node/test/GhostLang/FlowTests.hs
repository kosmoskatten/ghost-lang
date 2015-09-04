{-# LANGUAGE OverloadedStrings #-}
module GhostLang.FlowTests
    ( initStateTest
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
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.Text (Text)
import GhostLang (emptyNetworkConfiguration)
import GhostLang.API ( ProgramPath (..)
                     , NamedPattern (..)
                     , ExecParams (..)
                     , Resource (..)
                     , Service (..)
                     , PatternStatus (..)
                     )
import GhostLang.Node.Flow ( getHttpConfig
                           , setHttpConfig
                           , listPrograms
                           , listPatternsFromProgram
                           , loadProgram
                           , runNamedPattern
                           , listPatterns
                           , patternStatus
                           )
import GhostLang.Node.State ( NetworkConfiguration (..)
                            , ResourceKey
                            , initState 
                            )
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>), addExtension)
import System.Random (randomRIO)
import Test.HUnit
import qualified Data.Text as T

-- | Test some stuff about the initial state.
initStateTest :: Assertion
initStateTest = do
  state <- initState

  -- Http configuration shall be equal to what's given by the empty
  -- network configuration.
  initConfig <- getHttpConfig state
  emptyService @=? initConfig

  -- There shall be no programs in the initial state.
  initPrograms <- listPrograms state
  [] @=? initPrograms

  -- If trying to list the patterns for a non existing program there
  -- shall be Nothing as reply.
  patterns <- listPatternsFromProgram state "foo"
  Nothing @=? patterns

  -- There shall be no in flight patterns in the initial state.
  initPatterns <- listPatterns state
  [] @=? initPatterns

-- | Test that the setting of a new http
-- configuration is working as intended.
setHttpConfigTest :: Assertion
setHttpConfigTest = do
  state      <- initState
               
  let newHttp = Service { serviceAddress = "http://localhost"
                        , servicePort    = 8080 }
  setHttpConfig state newHttp
  newConfig <- getHttpConfig state
  newHttp @=? newConfig

-- | Try loading a non existing program. Shall fail.
loadNonExistingProgramTest :: Assertion
loadNonExistingProgramTest = do
  state <- initState
  
  result <- loadProgram state $ ProgramPath { programPath = "jsjsjsjsjsj" }
  case result of
    Left _ -> do
      programs <- listPrograms state
      [] @=? programs
    _      -> assertBool "Shall give a Left value" False

-- | Try loading a defunct program. Shall fail.
loadDefunctProgramTest :: Assertion
loadDefunctProgramTest = do
  state <- initState

  result <- withSourceProgram "Main.gl" defunctProgram $ loadProgram state
  case result of
    Left _ -> do
      programs <- listPrograms state
      [] @=? programs
    _      -> assertBool "Shall give a Left value" False

-- | Load a compilable program.
loadCompilableProgramTest :: Assertion
loadCompilableProgramTest = do
  state <- initState

  result <- withSourceProgram "Main.gl" compilableProgram $ loadProgram state
  case result of
    Right resource -> do

      -- Program shall have been registered with the expected resource.
      programs <- listPrograms state
      [resource] @=? programs

      -- There shall be possible to list the patterns, and they shall
      -- be 2.
      patterns <- listPatternsFromProgram state $ toResourceKey resource
      case patterns of
        Just xs -> 3 @=? length xs
        Nothing -> assertBool "Shall give a Just value" False

    _              -> assertBool "Shall give a Right value" False

-- | Try running a pattern from a non existing program. Shall fail.
runPatternFromNonExistingProgramTest :: Assertion
runPatternFromNonExistingProgramTest = do
  state <- initState
  result <- runNamedPattern state "nonexistingprogram" $ 
                namedPattern "nonexistingpattern"

  case result of
    Left _ -> do
      patterns <- listPatterns state
      [] @=? patterns
    _      -> assertBool "Shall give a Left value" False

-- | Try running a non existing pattern from a valid program. Shall
-- fail.
runNonExistingPatternTest :: Assertion
runNonExistingPatternTest = do
  state     <- initState
  Right url <- withSourceProgram "Main.gl" compilableProgram $ loadProgram state
  result    <- runNamedPattern state (toResourceKey url) $
                  namedPattern "nonexistingpattern"

  case result of
    Left _ -> do
      patterns <- listPatterns state
      [] @=? patterns
    _      -> assertBool "Shall give a Left value" False

-- | Run a named pattern.
runExistingPatternTest :: Assertion
runExistingPatternTest = do
  state     <- initState
  Right url <- withSourceProgram "Main.gl" compilableProgram $ loadProgram state
  result    <- runNamedPattern state (toResourceKey url) $
                  namedPattern "delay1"

  case result of
    Right pattern -> do
      patterns <- listPatterns state
      [pattern] @=? patterns
    _             -> assertBool "Shall give a right value" False

-- | List the pattern status for a non existing pattern.
patternStatusNonExistingPatternTest :: Assertion
patternStatusNonExistingPatternTest = do
  state  <- initState
  result <- patternStatus state "nonexistingpattern"

  case result of
    Nothing -> return ()
    _       -> assertBool "Shall return Nothing" False

-- | List the pattern status for a pattern that has failed.
patternStatusFailingPatternTest :: Assertion
patternStatusFailingPatternTest = do
  state      <- initState
  Right url  <- withSourceProgram "Main.gl" compilableProgram $ 
                  loadProgram state
  Right url' <- runNamedPattern state (toResourceKey url) $ namedPattern "fail"

  -- Short waiting time, 1/10 s, before querying the status.
  threadDelay 100000
  result <- patternStatus state (toResourceKey url')

  case result of
    Just status -> do
      True @=? completed status
      True @=? failed status
    Nothing     -> assertBool "Shall give a Just value" False

-- | List the pattern status for a pattern that has successfully
-- terminated.
patternStatusCompletedPatternTest :: Assertion
patternStatusCompletedPatternTest = do
  state      <- initState
  Right url  <- withSourceProgram "Main.gl" compilableProgram $
                  loadProgram state
  Right url' <- runNamedPattern state (toResourceKey url) $ 
                  namedPattern "delay2"

  -- | Short waiting time, 1/10 s, before querying the status.
  threadDelay 100000
  result <- patternStatus state (toResourceKey url')

  case result of
    Just status -> do
      True  @=? completed status
      False @=? failed status
    Nothing     -> assertBool "Shall give a Just value" False

-- | List the pattern status for a pattern that still is running.
patternStatusStillRunningTest :: Assertion
patternStatusStillRunningTest = do
  state      <- initState
  Right url  <- withSourceProgram "Main.gl" compilableProgram $
                  loadProgram state
  Right url' <- runNamedPattern state (toResourceKey url) $ 
                  namedPattern "delay1"

  -- | Short waiting time, 1/10 s, before querying the status.
  threadDelay 100000
  result <- patternStatus state (toResourceKey url')

  case result of
    Just status -> do
      False @=? completed status
      False @=? failed status
    Nothing     -> assertBool "Shall give a Just value" False

emptyService :: Service
emptyService =
    Service { serviceAddress = httpServiceAddress emptyNetworkConfiguration
            , servicePort    = httpServicePort emptyNetworkConfiguration }

-- | A ghost-program that won't compile.
defunctProgram :: String
defunctProgram = "kklmli"

-- | A compilable program. Delay patterns and one http pattern that
-- will fail on execution.
compilableProgram :: String
compilableProgram = 
    unlines $
    [ "module Main"
    , "pattern delay1 with weight 1"
    , "{"
    , "  Delay literal(1) sec"
    , "}"
    , ""
    , "pattern delay2 with weight 1"
    , "{"
    , "  Delay literal(1) usec"
    , "}"
    , ""
    , "pattern fail with weight 1"
    , "{"
    , "  Http GET [] literal(1) MB"
    , "}"
    ]

-- | Generates content in file <name>.<process id> in the system
-- temporary directory. Executes act and then removes the file.
withSourceProgram :: FilePath -> String -> (ProgramPath -> IO a) -> IO a
withSourceProgram name content act = 
    bracket genFile removeFile $ \f ->
        act ProgramPath { programPath = T.pack f }
    where
      genFile :: IO FilePath
      genFile = do
        tempDir <- getTemporaryDirectory
        rand <- randomRIO (0 :: Int, maxBound)
        let filePath = tempDir </> name `addExtension` show rand
        writeFile filePath content
        return filePath

toResourceKey :: Resource -> ResourceKey
toResourceKey = last . T.splitOn "/" . resourceUrl

namedPattern :: Text -> NamedPattern
namedPattern key = 
    NamedPattern { execPattern = key
                 , execParams  = ExecParams { shallTrace = False
                                            , srcIp      = Nothing }
                 }
