{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main 
    ( main
    ) where

import Command (Command (..), parseCommand)
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
              nc <- networkConfiguration <$> get
              liftIO $ runPattern pat [] nc mode
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
