{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.InterpreterTests
    ( simpleSequencePattern
    ) where

import Data.Text (pack)
import GhostLang.Counter (Counter (..), emptyCounter)
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

-- | Property to test a simple sequence pattern of instructions. The
-- instrInvoked counter shall be equal to the length of the operations
-- list.
simpleSequencePattern :: SimpleSequencePattern -> Property
simpleSequencePattern (SimpleSequencePattern p) =
    monadicIO $ do
      counter <- run (runInterpreter emptyCounter $ execPattern p)
      assert $ (opsLength p) == instrInvoked counter
    where
      opsLength (Pattern _ _ ops) = fromIntegral $ length ops
