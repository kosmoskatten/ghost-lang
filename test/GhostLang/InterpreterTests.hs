{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.InterpreterTests
    ( simpleSequencePattern
    ) where

import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Data.Text (pack)
import GhostLang.Counter ( Counter (..)
                         , emptyCounter
                         , getPatternRuns
                         , getTotalPatternRuns
                         , getTotalProcCalls
                         )
import GhostLang.Interpreter (execPattern)
import GhostLang.InterpreterM (runInterpreter)
import GhostLang.Types ( Label
                       , InstructionSet (..)
                       , Pattern (..)
                       , Operation (..)
                       )
import Test.QuickCheck
import Test.QuickCheck.Monadic

data TestInstrSet = Instr1 | Instr2
    deriving Show

instance Arbitrary TestInstrSet where
    arbitrary = elements [ Instr1, Instr2 ]

instance InstructionSet TestInstrSet where
    exec _ = return ()

data SimpleSequencePattern =
    SimpleSequencePattern (Pattern TestInstrSet)
    deriving Show

instance Arbitrary SimpleSequencePattern where
    arbitrary = SimpleSequencePattern <$> pattern'
        where
          pattern' :: Gen (Pattern TestInstrSet)
          pattern' = Pattern <$> arbitrary 
                             <*> arbitrary 
                             <*> (listOf invokeOperation)

instance Arbitrary Label where
    arbitrary = pack <$> (listOf1 $ elements ['a'..'z'])

invokeOperation :: Gen (Operation TestInstrSet)
invokeOperation = Invoke <$> arbitrary

-- | Property to test a simple sequence pattern of
-- instructions. There's a fresh counter instance at each invokation.
simpleSequencePattern :: SimpleSequencePattern -> Property
simpleSequencePattern (SimpleSequencePattern p) =
    monadicIO $ do
      inpC    <- run (newTVarIO emptyCounter)
      run (runInterpreter [inpC] $ execPattern p)
      counter <- run (readTVarIO inpC)

      -- As many instructions invoked as the number of operations in
      -- the list.
      assert $ (opsLength p) == instrInvoked counter

      -- This pattern shall have been run once.
      assert $ 1 == getPatternRuns (patternName p) counter

      -- The total number of patterns is one as well.
      assert $ 1 == getTotalPatternRuns counter

      -- No procedures called.
      assert $ 0 == getTotalProcCalls counter
    where
      opsLength (Pattern _ _ ops)    = fromIntegral $ length ops
      patternName (Pattern name _ _) = name
