{-# LANGUAGE OverloadedStrings #-}
module GhostLang.InterpreterTests
    ( oneLevelCallNoParamsPattern
    , oneLevelCallOneParamPattern
    , localScopeOneParamPattern
    , twoLevelTwoParamsPattern
    , longChainTwoParamsPattern
    ) where

import Control.Concurrent.STM (newTVarIO, readTVarIO)
import GhostLang.RuntimeState ( Counter (..)
                              , RuntimeState (..)
                              , defaultRuntimeState
                              , emptyCounter
                              , getProcCalls
                              , getTotalProcCalls
                              )
import GhostLang.Interpreter.InstructionSet (InstructionSet)
import GhostLang.InterpreterGenerators (TestInstrSet (..))
import GhostLang.Interpreter (runPattern')
import GhostLang.Types ( Value (..)
                       , Procedure (..)
                       , Pattern (..)
                       , Operation (..)
                       )
import Test.HUnit
import Text.Parsec.Pos (initialPos)

-- | Specific test case with a procedure call with no parameters.
oneLevelCallNoParamsPattern :: Assertion
oneLevelCallNoParamsPattern = do
  let p = Pattern (initialPos "") "pa1" 1
          [ Call (Procedure "pr1" []
                  [ Loop (Literal 5)
                      [ Invoke Instr1 ]
                  ]) []
          ]

  counter <- runWithInputCounter p

  1 @=? getProcCalls "pr1" counter
  1 @=? getTotalProcCalls counter
  1 @=? loopCmds counter
  5 @=? instrInvoked counter

-- | Specific test case with a procedure call with one parameter.
oneLevelCallOneParamPattern :: Assertion
oneLevelCallOneParamPattern = do
  let p = Pattern (initialPos "") "pa1" 1
          [ Call (Procedure "pr1" ["iterations"]
                  [ Loop (Stored "iterations")
                             [ Invoke Instr1 ]
                  ]) [ Literal 5 ]
          ]

  counter <- runWithInputCounter p

  1 @=? getProcCalls "pr1" counter
  1 @=? getTotalProcCalls counter
  1 @=? loopCmds counter
  5 @=? instrInvoked counter

-- | Specific test case to verify local scopes for each procedure
-- level. Using the same name in different procedured, but different
-- values, shall yield different values.
localScopeOneParamPattern :: Assertion
localScopeOneParamPattern = do
  let proc2 = Procedure "pr2" ["iterations"]
              [ Loop (Stored "iterations") [ Invoke Instr1 ]
              ]
      proc1 = Procedure "pr1" ["iterations"]
              [ Call proc2 [ Literal 10 ]
              , Loop (Stored "iterations") [ Invoke Instr2 ]
              ]
      p     = Pattern (initialPos "") "pa1" 1
              [ Call proc1 [ Literal 5 ]
              ]

  counter <- runWithInputCounter p

  1 @=? getProcCalls "pr1" counter
  1 @=? getProcCalls "pr2" counter
  2 @=? loopCmds counter

  -- The number of instructions invoked shall be 15 if the scoping is
  -- working correctly.
  15 @=? instrInvoked counter

-- | Specific test to verify that more than one parameter is handled
-- correctly.
twoLevelTwoParamsPattern :: Assertion
twoLevelTwoParamsPattern = do  
  let proc3 = Procedure "pr3" [] []
      proc2 = Procedure "pr2" [] []
      proc1 = Procedure "pr1" ["iterationsX", "iterationsY"]
              [ Loop (Stored "iterationsX") [ Call proc2 [] ]
              , Loop (Stored "iterationsY") [ Call proc3 [] ]
              ]
      p     = Pattern (initialPos "") "pa1" 1
              [ Call proc1 [ Literal 5, Literal 10 ]
              ] :: Pattern TestInstrSet

  counter <- runWithInputCounter p

  1  @=? getProcCalls "pr1" counter
  5  @=? getProcCalls "pr2" counter
  10 @=? getProcCalls "pr3" counter
  2  @=? loopCmds counter

-- | Specific test to verify a long chain of procedure calls and that
-- argument values are propagated as expected.
longChainTwoParamsPattern :: Assertion
longChainTwoParamsPattern = do
  let proc6 = Procedure "pr6" [] []
      proc5 = Procedure "pr5" [] []
      proc4 = Procedure "pr4" ["iterationsX", "iterationsD"]
              [ Loop (Stored "iterationsX") [ Call proc5 [] ]
              , Loop (Stored "iterationsD") [ Call proc6 [] ]
              ]
      proc3 = Procedure "pr3" ["iterationsX", "iterationsC"]
              [ Call proc4 [ Stored "iterationsX", Stored "iterationsC" ]
              ]
      proc2 = Procedure "pr2" ["iterationsX", "iterationsB"]
              [ Call proc3 [ Stored "iterationsX", Stored "iterationsB"]
              ]
      proc1 = Procedure "pr1" ["iterationsX", "iterationsA"]
              [ Call proc2 [ Stored "iterationsX", Stored "iterationsA"]
              ]
      p     = Pattern (initialPos "") "pa1" 1
              [ Call proc1 [ Literal 5, Literal 10 ]
              ] :: Pattern TestInstrSet
  
  counter <- runWithInputCounter p

  1  @=? getProcCalls "pr1" counter
  1  @=? getProcCalls "pr2" counter
  1  @=? getProcCalls "pr3" counter
  1  @=? getProcCalls "pr4" counter
  5  @=? getProcCalls "pr5" counter
  10 @=? getProcCalls "pr6" counter
  2  @=? loopCmds counter

runWithInputCounter :: InstructionSet a => Pattern a -> IO Counter
runWithInputCounter pattern = do
  inpC  <- newTVarIO emptyCounter
  state <- defaultRuntimeState
  runPattern' pattern $ state { counters = [inpC] }
  readTVarIO inpC
  
