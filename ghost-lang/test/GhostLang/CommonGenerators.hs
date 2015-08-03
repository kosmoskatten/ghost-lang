{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.CommonGenerators where

import GhostLang.Types ( Label
                       , ModuleSegment
                       , GhostModule (..)
                       , ModuleDecl (..)
                       , ImportDecl (..)
                       , TimeUnit (..)
                       , Value (..)
                       , Pattern (..)
                       , Procedure (..)
                       , Operation (..)
                       )
import GhostLang.Interpreter (IntrinsicSet (..))
import Test.QuickCheck
import Text.Parsec.Pos (initialPos)
import qualified Data.Text as T

-- | Arbitrary instance for GhostModule.
instance Arbitrary a => Arbitrary (GhostModule a) where
    arbitrary = GhostModule <$> arbitrary 
                            <*> (resize 15 $ listOf arbitrary)
                            <*> (resize 10 $ listOf arbitrary)
                            <*> (resize 10 $ listOf arbitrary)

-- | Arbitrary instance for ModuleDecl.
instance Arbitrary ModuleDecl where
    arbitrary = ModuleDecl <$> pure (initialPos "")
                           <*> listOf1 moduleSegment

-- | Arbitrary instance for ImportDecl.
instance Arbitrary ImportDecl where
    arbitrary = ImportDecl <$> listOf1 moduleSegment


-- | Arbitrary instance for IntrinsicSet.
instance Arbitrary IntrinsicSet where
    arbitrary = Delay <$> arbitrary

-- | Arbitrary instance for TimeUnit.
instance Arbitrary TimeUnit where
    arbitrary = oneof [ USec <$> arbitrary
                      , MSec <$> arbitrary
                      , Sec  <$> arbitrary
                      ]

-- | Arbitrary instance for Value.
instance Arbitrary Value where
    arbitrary = oneof [ literalValue, storedValue, gaussianValue, uniformValue ]
        where
          literalValue  = Literal  <$> nonNegative
          storedValue   = Stored   <$> validId
          gaussianValue = Gaussian <$> nonNegative <*> nonNegative
          uniformValue  = Uniform  <$> nonNegative <*> nonNegative
          nonNegative   = choose (0, maxBound)

-- | Arbitrary instance for Pattern.
instance Arbitrary a => Arbitrary (Pattern a) where
    arbitrary = Pattern <$> pure (initialPos "")
                        <*> validId 
                        <*> choose (0, maxBound) 
                        <*> (resize 15 $ listOf arbitrary)

-- | Arbitrary instance for Procedure.
instance Arbitrary a => Arbitrary (Procedure a) where
    arbitrary = Procedure <$> validId
                          <*> listOf validId
                          <*> (resize 15 $ listOf arbitrary)

-- | Arbitrary instance for Operation.
instance Arbitrary a => Arbitrary (Operation a) where
    arbitrary = oneof [ invokeOp, loopOp, concurrentOp, unresolvedOp ]
        where
          invokeOp     = Invoke <$> arbitrary
          loopOp       = Loop <$> arbitrary <*> listOf invokeOp
          concurrentOp = Concurrently <$> listOf invokeOp
          unresolvedOp = Unresolved <$> pure (initialPos "")
                                    <*> validId 
                                    <*> listOf arbitrary

validId :: Gen Label
validId = T.pack <$> validId'
    where validId' = (:) <$> elements ['a'..'z']
                         <*> listOf (elements $ ['a'..'z'] ++
                                                ['A'..'Z'] ++
                                                ['0'..'9'])

moduleSegment :: Gen ModuleSegment
moduleSegment = (:) <$> elements ['A'..'Z']
                    <*> listOf (elements $ ['a'..'z'] ++ ['0'..'9'] ++ "-_")
