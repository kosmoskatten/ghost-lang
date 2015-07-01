{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.Generators 
    ( TestInstrSet (..)
    , SimpleSequencePattern (..)
    , ManySimpleSequencePatterns (..)
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
                             , Unresolved <$> arbitrary
                             ]

-- | Arbitrary instance for Operation.
instance Arbitrary a => Arbitrary (Operation a) where
    arbitrary = oneof [ Invoke <$> arbitrary
                      , Loop <$> arbitrary <*> (listOf (Invoke <$> arbitrary))
                      , Call <$> arbitrary
                      , Unresolved <$> arbitrary
                      ]

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

-- | Generate a label with at least length 1.
instance Arbitrary Label where
    arbitrary = pack <$> (listOf1 $ elements fromValidChars)
        where fromValidChars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "_.-"

invokeOperation :: Gen (Operation TestInstrSet)
invokeOperation = Invoke <$> arbitrary
