{-# LANGUAGE OverloadedStrings #-}
module GhostLang.InterpreterTests
    ( oneLevelCallNoParamsPattern
    , oneLevelCallOneParamPattern
    ) where

import Control.Concurrent.STM (newTVarIO, readTVarIO)
import GhostLang.Counter ( Counter (..)
                         , emptyCounter
                         , getProcCalls
                         , getTotalProcCalls
                         )
import GhostLang.Generators (TestInstrSet (..))
import GhostLang.Interpreter (execPattern)
import GhostLang.InterpreterM (runInterpreter)
import GhostLang.Types ( Value (..)
                       , Procedure (..)
                       , Pattern (..)
                       , Operation (..)
                       )
import Test.HUnit

-- | Specific test case with a procedure call with no parameters.
oneLevelCallNoParamsPattern :: Assertion
oneLevelCallNoParamsPattern = do
  let p = Pattern "pa1" 1
          [ Call (Procedure "pr1" []
                  [ Loop (Literal 5)
                      [ Invoke Instr1 ]
                  ]) []
          ]
  inpC    <- newTVarIO emptyCounter
  runInterpreter [inpC] $ execPattern p
  counter <- readTVarIO inpC

  1 @=? getProcCalls "pr1" counter
  1 @=? getTotalProcCalls counter
  1 @=? loopCmds counter
  5 @=? instrInvoked counter

-- | Specific test case with a procedure call with one parameter.
oneLevelCallOneParamPattern :: Assertion
oneLevelCallOneParamPattern = do
  let p = Pattern "pa1" 1
          [ Call (Procedure "pr1" ["iterations"]
                  [ Loop (Stored "iterations")
                             [ Invoke Instr1 ]
                  ]) [ Literal 5 ]
          ]
  inpC    <- newTVarIO emptyCounter
  runInterpreter [inpC] $ execPattern p
  counter <- readTVarIO inpC

  1 @=? getProcCalls "pr1" counter
  1 @=? getTotalProcCalls counter
  1 @=? loopCmds counter
  5 @=? instrInvoked counter
