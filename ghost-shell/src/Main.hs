{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main 
    ( main
    ) where

import Command (Command (..), parseCommand)
import Shell ( Shell
             , runShell
             , nodeLoadProgram
             , nodeListProgram
             , storeProgramResource
             , liftIO
             )
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import qualified Data.Text as T

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
  case result of
    Left  err -> liftIO $ printf "Error: %s\n" err
    Right res -> do
        liftIO $ printf "Saving resource: %s\n" (T.unpack res)
        storeProgramResource res
  repl

-- | List the patterns from the saved program.
eval ListProgram = do
  result <- nodeListProgram
  case result of
    Left err  -> liftIO $ printf "Error: %s\n" err
    Right res -> liftIO $ printf "Patterns: %s\n" (show res)
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

eval c = do
  liftIO $ printf "Command '%s' not yet implemented\n" (show c)
  repl

{-
import Command (Command (..), parseCommand)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Monad (forM_)
import Control.Monad.State.Strict ( StateT
                                  , MonadState
                                  , evalStateT
                                  , get
                                  , modify' )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import GhostLang ( GhostProgram
                 , NetworkConfiguration (..)
                 , compileAndLink
                 , emptyCounter
                 , emptyNetworkConfiguration
                 , toPatternList
                 , runPattern
                 )
import System.IO (hFlush, stdout)
import Text.Printf (printf)

type LoadedProgram = (FilePath, GhostProgram)

data State = State { loadedProgram        :: !(Maybe LoadedProgram) 
                   , networkConfiguration :: !NetworkConfiguration }

-- | Monad stack for the ghost-shell.
newtype Shell a = Shell { extrShell :: StateT State IO a }
    deriving (Functor, Applicative, Monad, MonadState State, MonadIO)

main :: IO ()
main = runShell repl

-- | Entry point for the read-eval-print-loop.
repl :: Shell ()
repl = do
  liftIO $ putStr "> " >> hFlush stdout
  eval =<< parseCommand <$> liftIO getLine

-- | Evaluate a command.
eval :: Command -> Shell ()

eval (LoadProgram path) = do
  maybeProgram <- liftIO $ compileAndLink path
  case maybeProgram of
    Right prog  -> do 
      setLoadedProgram (path, prog)
      liftIO $ printf "Loaded '%s'\n" path
    Left msg -> liftIO $ printf "Error: %s\n" msg
  repl

-- | Print shell status.
eval Status = do
  loadedProgram' <- loadedProgram <$> get
  if isJust loadedProgram' then
      liftIO $ printf "Loaded program: '%s'\n" (fst $ fromJust loadedProgram')
  else liftIO $ printf "No program loaded\n"

  repl

-- | List information from the current state.
eval ListInfo = do
  networkConfiguration' <- networkConfiguration <$> get
  liftIO $ printf "Http service address: %s\n" 
                  (httpServiceAddress networkConfiguration')
  liftIO $ printf "Http service port: %d\n" 
                  (httpServicePort networkConfiguration')

  loadedProgram' <- loadedProgram <$> get
  if isJust loadedProgram' then 
      do liftIO $ printf "Pattern : Weight\n"
         forM_ (toPatternList $ snd (fromJust loadedProgram')) $ \(l, w, _) ->
             liftIO $ printf "%s : %ld\n" (T.unpack l) w
  else liftIO $ printf "No program loaded\n"
  
  repl

-- | Set http parameters in the state.
eval (SetHttpParams service port) = do
    modify' $ \s -> 
        s { networkConfiguration = 
                NetworkConfiguration { httpServiceAddress = service
                                     , httpServicePort    = port }
          }

    repl

-- | Run the selected pattern.
eval (RunPattern pattern mode) = do
  loadedProgram' <- loadedProgram <$> get
  case loadedProgram' of
    Just (_, prog) -> do
        let plist    = toPatternList prog
            maybePat = find (\(p, _, _) -> p == T.pack pattern) plist
        case maybePat of
          Just (_, _, pat) -> do
              nc  <- networkConfiguration <$> get
              cnt <- liftIO $ newTVarIO emptyCounter
              liftIO $ runPattern pat [cnt] nc mode
              cnt' <- liftIO $ readTVarIO cnt
              liftIO $ printf "Counter set contents: %s\n" (show cnt')
          Nothing  -> liftIO $ printf "Cannot find pattern '%s'\n" pattern
    Nothing -> liftIO $ printf "No program loaded\n"

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

runShell :: Shell a -> IO a
runShell act = evalStateT (extrShell act) emptyState

emptyState :: State
emptyState = State { loadedProgram        = Nothing
                   , networkConfiguration = emptyNetworkConfiguration }

setLoadedProgram :: LoadedProgram -> Shell ()
setLoadedProgram p = modify' $ \s -> s { loadedProgram = Just p }

-}
