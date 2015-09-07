module GhostLang.Interpreter
    ( IntrinsicSet (..)
    , runPattern'
    ) where

import GhostLang.Interpreter.InterpreterM (runInterpreter)
import GhostLang.Interpreter.Intrinsic (IntrinsicSet (..))
import GhostLang.Interpreter.InstructionSet (InstructionSet, execPattern)
import GhostLang.RuntimeState (RuntimeState)
import GhostLang.Types (Pattern)

-- | Run a selected pattern with a state.
runPattern' :: InstructionSet a => Pattern a -> RuntimeState -> IO ()
runPattern' pattern state = runInterpreter state $ execPattern pattern

