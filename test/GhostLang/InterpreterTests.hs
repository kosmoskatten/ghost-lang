{-# LANGUAGE OverloadedStrings #-}
module GhostLang.InterpreterTests
    ( simpleInvokeTest
    ) where

import GhostLang.Interpreter ( Interpreter
                             , Executable (..)
                             , Operation (..)
                             , Pattern (..)
                             , execPattern
                             , runInterpreter
                             )
import Test.HUnit

data TestDummy = TestDummy

instance Executable TestDummy where
    exec TestDummy = return ()

simpleInvokeTest :: Assertion
simpleInvokeTest = do
  runInterpreter execPattern $ Pattern "" 1 [ Invoke $ TestDummy ]
