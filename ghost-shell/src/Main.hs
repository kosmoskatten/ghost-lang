{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main 
    ( main
    ) where

import Data.Aeson.Encode.Pretty (encodePretty)
import Command (Command (..), parseCommand)
import GhostLang.API (ToJSON)
import Shell ( Shell
             , runShell
             , nodeGetHttpConfig
             , nodeSetHttpConfig
             , nodeLoadProgram
             , nodeListSelectedProgram
             , nodeListPrograms
             , nodeListPatterns
             , nodeRunNamedPattern
             , nodeRunRandomPattern
             , storeProgramResource
             , liftIO
             )
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
  [nodeAddress] <- getArgs
  runShell repl nodeAddress

-- | Entry point for the read-eval-print-loop.
repl :: Shell ()
repl = do
  liftIO $ putStr "> " >> hFlush stdout
  eval =<< parseCommand <$> liftIO getLine

-- | Evaluate a command.
eval :: Command -> Shell ()

-- | Load a program.
eval (LoadProgram path) = do
  result <- nodeLoadProgram path
  presentJsonBody result
  either (const $ return ()) storeProgramResource result
  repl

-- | List the patterns from the saved program.
eval ListSelectedProgram = do
  presentJsonBody =<< nodeListSelectedProgram
  repl

-- | List the programs available on the node.
eval ListPrograms = do
  presentJsonBody =<< nodeListPrograms
  repl

-- | List the patterns in-flight in the node.
eval ListPatterns = do
  presentJsonBody =<< nodeListPatterns
  repl

-- | Get the http configuration of the node.
eval GetHttpConfig = do
  presentJsonBody =<< nodeGetHttpConfig
  repl

-- | Set the http configuration of the node.
eval (SetHttpConfig server port) = do
  result <- nodeSetHttpConfig server port
  case result of
    Right () -> liftIO $ putStrLn "OK"
    Left err -> liftIO $ printf "Error: %s\n" err
  repl

-- | Run a named pattern from the saved program.
eval (RunNamedPattern name trace src) = do
  presentJsonBody =<< nodeRunNamedPattern name trace src
  repl

-- | Run a named pattern from the saved program.
eval (RunRandomPattern trace src) = do
  presentJsonBody =<< nodeRunRandomPattern trace src
  repl

-- | Print help information.
eval Help = do
  liftIO $ putStrLn "Help ..."
  repl

-- | Quit the repl loop.
eval Quit = do
  liftIO $ putStrLn "Bye!"
  return ()

-- | Just an empty line.
eval EmptyLine = repl

-- | Unknown stuff ...
eval (Unknown str) = do
  liftIO $ putStrLn str
  repl

presentJsonBody :: ToJSON a => Either String a -> Shell ()
presentJsonBody result = 
  case result of
    Right reply -> liftIO $ printf "%s\n" (encodePretty'String reply)
    Left err    -> liftIO $ printf "Error: %s\n" err

encodePretty'String :: ToJSON a => a -> String
encodePretty'String = BS.unpack . encodePretty
