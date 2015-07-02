{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.Interpreter
    ( InstructionSet (..)
    , execPattern
    ) where

import Control.Monad (replicateM_)
import GhostLang.InterpreterM ( InterpreterM
                              , incInstrInvoked
                              , incPatternRuns
                              , incLoopRuns
                              , evalValue
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
      incLoopRuns
      times' <- fromIntegral <$> evalValue times
      replicateM_ times' $ mapM_ exec ops

    exec _ = undefined
