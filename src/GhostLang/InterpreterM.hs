{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GhostLang.InterpreterM
    ( InterpreterM (..)
    , runInterpreter
    ) where

import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, execStateT)
import Control.Monad.IO.Class (MonadIO)
import GhostLang.Counter (Counter (..))

-- | Interpreter monad type for interpretation of instructions
-- implementing the InstructionSet type class.
newtype InterpreterM a = 
    InterpreterM { extractInterpreterM :: ReaderT Int (StateT Counter IO) a }
  deriving (Functor, Applicative, Monad
           , MonadReader Int, MonadState Counter, MonadIO)

-- | Run an InterpreterM action. The action will be supplied the given
-- counter and an empty reader set.
runInterpreter :: Counter -> InterpreterM () -> IO Counter
runInterpreter counter interpreter =
  execStateT (runReaderT (extractInterpreterM interpreter) 0) counter
