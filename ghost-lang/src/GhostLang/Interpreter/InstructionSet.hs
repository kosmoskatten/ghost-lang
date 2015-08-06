{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.Interpreter.InstructionSet
    ( InstructionSet (..)
    , execPattern
    ) where

import Control.Concurrent.Async (async, wait)
import Control.Monad (replicateM_)
import Data.Maybe (fromJust)
import GhostLang.Interpreter.InterpreterM ( InterpreterM
                                          , runInterpreter
                                          , incInstrInvoked
                                          , incPatternRuns
                                          , incLoopCmds
                                          , incConcCmds
                                          , incProcCalls
                                          , evalValue
                                          , trace
                                          , ask
                                          , get
                                          , local
                                          , liftIO
                                          )
import GhostLang.Interpreter.Scope (fromList, lookup)
import GhostLang.RuntimeState (RuntimeState)
import GhostLang.Types ( Label
                       , Value (..)
                       , Operation (..)
                       , Pattern (..)
                       , Procedure (..)
                       )
import Prelude hiding (lookup)
import Text.Printf (printf)

import qualified Data.Text as T

-- | Instruction set type class.
class InstructionSet a where
    exec :: a -> InterpreterM ()

-- | Execute a pattern within InterpreterM.
execPattern :: InstructionSet a => Pattern a -> InterpreterM ()
execPattern (Pattern _ name _ ops) = do
  trace $ printf "Enter pattern '%s'" (T.unpack name)
  incPatternRuns name
  mapM_ exec ops
  trace $ printf "Exit pattern '%s'" (T.unpack name)

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

      trace $ printf "Enter proc '%s'" (T.unpack procName)

      -- Copy the values and name them correctly into a new
      -- scope. Some values may need to be copied from the current
      -- scope.
      scope <- fromList <$> mapM copyValue pairs
                     
      incProcCalls procName
      
      -- Run the procedure inside its new scope.
      local (const scope) $ mapM_ exec ops
      trace $ printf "Exit proc '%s'" (T.unpack procName)
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
concurrently :: InstructionSet a => RuntimeState -> [Operation a] -> IO ()
concurrently state ops = do
  as <- mapM (async . runInterpreter state . execOperation) ops
  mapM_ wait as
