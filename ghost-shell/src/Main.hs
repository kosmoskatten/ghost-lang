{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main 
    ( main
    ) where

import Command (Command (..), parseCommand)
import Documentation (commandDocs)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.IORef (IORef, newIORef)
import GhostLang.API (ToJSON)
import Shell ( State
             , mkState
             , nodeGetHttpConfig
             , nodeSetHttpConfig
             , nodeLoadProgram
             , nodeListSelectedProgram
             , nodeListPrograms
             , nodeListPatterns
             , nodeRunNamedPattern
             , nodeRunRandomPattern
             , nodeListGlobalCounter
             , nodeListSelectedCounter
             , nodeListSelectedStatus
             , storeProgramResource
             )
import System.Console.Haskeline ( InputT
                                , defaultSettings
                                , runInputT
                                , getInputLine
                                , outputStrLn
                                )
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
  [nodeAddress] <- getArgs
  state         <- newIORef =<< mkState nodeAddress
  runInputT defaultSettings $ repl state
  
repl :: IORef State -> InputT IO ()
repl state = do
  input <- getInputLine "> "
  case input of
    Just line -> eval state $ parseCommand line
    Nothing   -> return ()

-- | Evaluate a command.
eval :: IORef State -> Command -> InputT IO ()

-- | Load a program.
eval state (LoadProgram path) = do
  result <- nodeLoadProgram state path
  presentJsonBody result
  either (const $ return ()) (storeProgramResource state) result
  repl state

-- | List the patterns from the saved program.
eval state ListSelectedProgram = do
  presentJsonBody =<< nodeListSelectedProgram state
  repl state

-- | List the programs available on the node.
eval state ListPrograms = do
  presentJsonBody =<< nodeListPrograms state
  repl state

-- | List the patterns in-flight in the node.
eval state ListPatterns = do
  presentJsonBody =<< nodeListPatterns state
  repl state

-- | Get the http configuration of the node.
eval state GetHttpConfig = do
  presentJsonBody =<< nodeGetHttpConfig state
  repl state

-- | Set the http configuration of the node.
eval state (SetHttpConfig server port) = do
  result <- nodeSetHttpConfig state server port
  case result of
    Right () -> outputStrLn "OK"
    Left err -> outputStrLn $ printf "Error: %s" err
  repl state

-- | Run a named pattern from the saved program.
eval state (RunNamedPattern name trace src) = do
  presentJsonBody =<< nodeRunNamedPattern state name trace src
  repl state

-- | Run a named pattern from the saved program.
eval state (RunRandomPattern trace src) = do
  presentJsonBody =<< nodeRunRandomPattern state trace src
  repl state

-- | List the global counter.
eval state ListGlobalCounter = do
  presentJsonBody =<< nodeListGlobalCounter state
  repl state

-- | List a selected counter.
eval state (ListSelectedCounter res) = do
  presentJsonBody =<< nodeListSelectedCounter state res
  repl state

-- | List a selected status.
eval state (ListSelectedStatus res) = do
  presentJsonBody =<< nodeListSelectedStatus state res
  repl state

-- | Print help information.
eval state Help = do
  outputStrLn $ unlines commandDocs
  repl state

-- | Quit the repl loop.
eval _state Quit = do
  outputStrLn "Bye"
  return ()

-- | Just an empty line.
eval state EmptyLine = repl state

-- | Unknown stuff ...
eval state (Unknown str) = do
  outputStrLn str
  repl state

presentJsonBody :: ToJSON a => Either String a -> InputT IO ()
presentJsonBody result = 
  case result of
    Right reply -> outputStrLn $ printf "%s" (encodePretty'String reply)
    Left err    -> outputStrLn $ printf "Error: %s" err

encodePretty'String :: ToJSON a => a -> String
encodePretty'String = BS.unpack . encodePretty
