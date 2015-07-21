{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.CommonGenerators where

import GhostLang.Types (Label, TimeUnit (..), Value (..))
import GhostLang.Intrinsic (IntrinsicSet (..))
import Test.QuickCheck
import qualified Data.Text as T

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

validId :: Gen Label
validId = T.pack <$> validId'
    where validId' = (:) <$> elements ['a'..'z']
                         <*> listOf (elements $ ['a'..'z'] ++
                                                ['A'..'Z'] ++
                                                ['0'..'9'])
