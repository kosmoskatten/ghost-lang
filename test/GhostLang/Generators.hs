{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.Generators 
    ( TestInstrSet (..)
    , SimpleSequencePattern (..)
    , ManySimpleSequencePatterns (..)
    , NonNestedLoopPattern (..)
    , NonNestedConcPattern (..)
    ) where

import Data.Serialize (Serialize (..))
import Data.Text (pack)
import GHC.Generics (Generic)
import GHC.Int (Int64)
import GhostLang.Interpreter (InstructionSet (..))
import GhostLang.Types ( Label
                       , Value (..)
                       , Program (..)
                       , Pattern (..)
                       , Procedure (..)
                       , Operation (..)
                       )
import Test.QuickCheck

-- | Arbitrary instance for Value.
instance Arbitrary Value where
    arbitrary = oneof [ Const <$> posInt
                      , Uniform <$> posInt <*> posInt
                      , Gaussian <$> posInt <*> posInt
                      , Ind <$> arbitrary
                      ]
        where
          posInt :: Gen Int64
          posInt = choose (0, maxBound)

-- | Arbitrary instance for Program.
instance Arbitrary a => Arbitrary (Program a) where
    arbitrary = Program <$> arbitrary <*> (listOf arbitrary)

-- | Arbitrary instance for Pattern.
instance Arbitrary a => Arbitrary (Pattern a) where
    arbitrary = Pattern <$> arbitrary <*> arbitrary <*> (listOf arbitrary)

-- | Arbitrary instance for Procedure. Limited in randomness to avoid
-- infinite recursion.
instance Arbitrary a => Arbitrary (Procedure a) where
    arbitrary = Procedure <$> arbitrary <*> (listOf noCall)
        where noCall = oneof [ Invoke <$> arbitrary
                             , Loop <$> arbitrary 
                                    <*> (listOf (Invoke <$> arbitrary))
                             , Concurrently <$> (listOf (Invoke <$> arbitrary))
                             , Unresolved <$> arbitrary
                             ]

-- | Arbitrary instance for Operation.
instance Arbitrary a => Arbitrary (Operation a) where
    arbitrary = oneof [ Invoke <$> arbitrary
                      , Loop <$> arbitrary <*> (listOf (Invoke <$> arbitrary))
                      , Concurrently <$> (listOf (Invoke <$> arbitrary))
                      , Call <$> arbitrary
                      , Unresolved <$> arbitrary
                      ]

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
simpleSequencePattern = Pattern <$> arbitrary
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
          pattern' = Pattern <$> arbitrary
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
          pattern = Pattern <$> arbitrary
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
      value = Const <$> choose (0, 10)
