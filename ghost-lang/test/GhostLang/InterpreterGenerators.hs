{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.InterpreterGenerators 
    ( TestInstrSet (..)
    , SimpleSequencePattern (..)
    , ManySimpleSequencePatterns (..)
    , NonNestedLoopPattern (..)
    , NonNestedConcPattern (..)
    ) where

import Data.Serialize (Serialize (..))
import Data.Text (pack)
import GHC.Generics (Generic)
import GhostLang.CommonGenerators ()
import GhostLang.InstructionSet (InstructionSet (..))
import GhostLang.Types ( Label
                       , Value (..)
                       , Program (..)
                       , Pattern (..)
                       , Operation (..)
                       )
import Test.QuickCheck
import Text.Parsec.Pos (initialPos)

-- | Arbitrary instance for Program.
instance Arbitrary a => Arbitrary (Program a) where
    arbitrary = Program <$> (listOf arbitrary)

-- | Generate a label with at least length 1.
instance Arbitrary Label where
    arbitrary = pack <$> (listOf1 $ elements fromValidChars)
        where fromValidChars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "_.-"

-- | Simple instruction set for testing purposes. The instructions
-- shall have no side effects.
data TestInstrSet = Instr1 | Instr2
    deriving (Eq, Generic, Show)

instance Serialize TestInstrSet

instance Arbitrary TestInstrSet where
    arbitrary = elements [ Instr1, Instr2 ]

instance InstructionSet TestInstrSet where
    exec _ = return ()

-- | Test case wrapper type. A simple sequence pattern is a pattern
-- with only plain TestInstrSet instructions invoked.
data SimpleSequencePattern =
    SimpleSequencePattern (Pattern TestInstrSet)
    deriving Show

instance Arbitrary SimpleSequencePattern where
    arbitrary = SimpleSequencePattern <$> simpleSequencePattern

-- | Test case wrapper type. Many simple sequence patterns. See
-- SimpleSequencePattern for explanation.
data ManySimpleSequencePatterns =
    ManySimpleSequencePatterns [Pattern TestInstrSet]
    deriving Show

instance Arbitrary ManySimpleSequencePatterns where
    arbitrary = ManySimpleSequencePatterns <$> (listOf simpleSequencePattern)

simpleSequencePattern :: Gen (Pattern TestInstrSet)
simpleSequencePattern = Pattern <$> pure (initialPos "")
                                <*> arbitrary
                                <*> arbitrary
                                <*> (listOf invokeOperation)

-- | Test case wrapper type. A sequence with simple instructions and
-- non nested loops.
data NonNestedLoopPattern =
    NonNestedLoopPattern (Pattern TestInstrSet)
    deriving Show

instance Arbitrary NonNestedLoopPattern where
    arbitrary = NonNestedLoopPattern <$> pattern'
        where
          pattern' = Pattern <$> pure (initialPos "")
                             <*> arbitrary
                             <*> arbitrary
                             <*> (listOf $ oneof [ invokeOperation
                                                 , nonNestedLoop
                                                 ])

-- | Test case wrapper type. A sequence with simple instructions and
-- non nested concurrency sections.
data NonNestedConcPattern =
    NonNestedConcPattern (Pattern TestInstrSet)
    deriving Show

instance Arbitrary NonNestedConcPattern where
    arbitrary = NonNestedConcPattern <$> pattern
        where
          pattern = Pattern <$> pure (initialPos "")
                            <*> arbitrary
                            <*> arbitrary
                            <*> (listOf $ oneof [ invokeOperation
                                                , nonNestedConc
                                                ])
          nonNestedConc = Concurrently <$> (listOf invokeOperation)

invokeOperation :: Gen (Operation TestInstrSet)
invokeOperation = Invoke <$> arbitrary

nonNestedLoop :: Gen (Operation TestInstrSet)
nonNestedLoop = Loop <$> value
                     <*> (listOf invokeOperation)
    where
      value = Literal <$> choose (0, 10)
