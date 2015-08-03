module GhostLang.Interpreter
    ( IntrinsicSet (..)
    , runPattern'
    ) where

import GhostLang.Interpreter.InterpreterM
import GhostLang.Interpreter.Intrinsic
import GhostLang.Interpreter.InstructionSet
import GhostLang.Types (Pattern)
import GhostLang.RuntimeState ( RuntimeState (..)
                              , TVar
                              , Counter                                
                              , Mode
                              , NetworkConfiguration )

runPattern' :: InstructionSet a 
            => Pattern a 
            -> [TVar Counter] 
            -> NetworkConfiguration
            -> Mode
            -> IO ()
runPattern' p cs nw m = do
  let state = RuntimeState { counters             = cs
                           , networkConfiguration = nw
                           , mode                 = m }
  runInterpreter state $ execPattern p
