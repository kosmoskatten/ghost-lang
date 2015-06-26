{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.Interpreter
    ( execPattern
    ) where

import GhostLang.InterpreterM (InterpreterM)
import GhostLang.Types (InstructionSet (..), Operation (..), Pattern (..))

-- | Execute a pattern within InterpreterM.
execPattern :: InstructionSet a => Pattern a -> InterpreterM ()
execPattern (Pattern _ _ ops) = mapM_ exec ops

-- | Implementation of the InstructionSet for the Operation level
-- instructions.
instance InstructionSet a => InstructionSet (Operation a) where
    exec (Invoke instr) = exec instr
