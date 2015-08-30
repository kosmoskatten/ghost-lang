{-# LANGUAGE OverloadedStrings #-}
module GhostLang.FlowTests
    ( initStateTest
    , setHttpConfigTest
    , loadNonExistingProgramTest
    , loadDefunctProgramTest
    , loadCompilableProgramTest
    ) where

import Control.Exception (bracket)
import GhostLang (emptyNetworkConfiguration)
import GhostLang.API ( ProgramPath (..)
                     , Resource (..)
                     , Service (..)
                     )
import GhostLang.Node.Flow ( getHttpConfig
                           , setHttpConfig
                           , listPrograms
                           , listPatternsFromProgram
                           , loadProgram
                           , listPatterns
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
        Just xs -> 2 @=? length xs
        Nothing -> assertBool "Shall give a Just value" False

    _              -> assertBool "Shall give a Right value" False

emptyService :: Service
emptyService =
    Service { serviceAddress = httpServiceAddress emptyNetworkConfiguration
            , servicePort    = httpServicePort emptyNetworkConfiguration }

-- | A ghost-program that won't compile.
defunctProgram :: String
defunctProgram = "kklmli"

-- | A compilable program with only delay patterns.
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
