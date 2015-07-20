{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.Interpreter
    ( InstructionSet (..)
    , execPattern
    ) where

import Control.Concurrent.Async (async, wait)
import Control.Monad (replicateM_)
import Data.Maybe (fromJust)
import GhostLang.InterpreterM ( InterpreterM
                              , State
                              , runInterpreter
                              , incInstrInvoked
                              , incPatternRuns
                              , incLoopCmds
                              , incConcCmds
                              , incProcCalls
                              , evalValue
                              , ask
                              , get
                              , local
                              , liftIO
                              )
import GhostLang.Types ( Label
                       , Value (..)
                       , Operation (..)
                       , Pattern (..)
                       , Procedure (..)
                       )
import GhostLang.Scope (fromList, lookup)
import Prelude hiding (lookup)

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

-- | Implementation of the InstructionSet interpreter for the Operation level
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

    -- Invoke a procedure call. Update counters accordingly.
    exec (Call proc argValues) = do
      -- The procedure itself carries a list of names for its
      -- arguments. The call is carrying a list of values. Zip'em to
      -- form a list of pairs.
      let Procedure procName argNames ops = proc
          pairs = argNames `zip` argValues

      -- Copy the values and name them correctly into a new
      -- scope. Some values may need to be copied from the current
      -- scope.
      scope <- fromList <$> mapM copyValue pairs

      incProcCalls procName
      
      -- Run the procedure inside its new scope.
      local (const scope) $ mapM_ exec ops
          where
            copyValue :: (Label, Value) -> InterpreterM (Label, Value)
            copyValue (label, Stored label') = do
                -- Stored value. Copy value from current scope.
                value' <- fromJust . lookup label' <$> ask
                return (label, value')
            copyValue v = return v

    exec _ = undefined

-- | Run a set of operations concurrently. Each operation is started
-- in its own monad stack instance.
concurrently :: InstructionSet a => State -> [Operation a] -> IO ()
concurrently state ops = do
  as <- mapM (async . runInterpreter state . execOperation) ops
  mapM_ wait as
