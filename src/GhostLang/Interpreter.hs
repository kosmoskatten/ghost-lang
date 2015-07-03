{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.Interpreter
    ( InstructionSet (..)
    , execPattern
    ) where

import Control.Concurrent.Async (async, wait)
import Control.Monad (replicateM_)
import GhostLang.InterpreterM ( InterpreterM
                              , State
                              , runInterpreter
                              , incInstrInvoked
                              , incPatternRuns
                              , incLoopCmds
                              , incConcCmds
                              , evalValue
                              , get
                              , liftIO
                              )
import GhostLang.Types (Operation (..), Pattern (..))

-- | Instruction set type class.
class InstructionSet a where
    exec :: a -> InterpreterM ()

-- | Execute a pattern within InterpreterM.
execPattern :: InstructionSet a => Pattern a -> InterpreterM ()
execPattern (Pattern name _ ops) = do
  incPatternRuns name
  mapM_ exec ops

execOperation :: InstructionSet a => Operation a -> InterpreterM ()
execOperation = exec

-- | Implementation of the InstructionSet for the Operation level
-- instructions.
instance InstructionSet a => InstructionSet (Operation a) where
    -- Invoke one instruction from the instruction set. Update
    -- counters accordingly.
    exec (Invoke instr) = do
      incInstrInvoked
      exec instr

    -- Invoke a loop. Update counters accordingly.
    exec (Loop times ops) = do
      incLoopCmds
      times' <- fromIntegral <$> evalValue times
      replicateM_ times' $ mapM_ exec ops

    -- Invoke a concurrent command. Update counters accordingly.
    exec (Concurrently ops) = do
      incConcCmds
      state <- get
      liftIO $ concurrently state ops

    exec _ = undefined

-- | Run a set of operations concurrently. Each operation is started
-- in its own monad stack instance.
concurrently :: InstructionSet a => State -> [Operation a] -> IO ()
concurrently state ops = do
  as <- mapM (async . runInterpreter state . execOperation) ops
  mapM_ wait as
