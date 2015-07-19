{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.CommonGenerators where

import GhostLang.Types (Label, Value (..))
import Test.QuickCheck
import qualified Data.Text as T

-- | Arbitrary instance for Value.
instance Arbitrary Value where
    arbitrary = oneof [ literalValue, storedValue, gaussianValue, uniformValue ]
        where
          literalValue  = Literal  <$> choose (0, maxBound)
          storedValue   = Stored   <$> validId
          gaussianValue = Gaussian <$> choose (0, maxBound) 
                                   <*> choose (0, maxBound)
          uniformValue  = Uniform  <$> choose (0, maxBound)
                                   <*> choose (0, maxBound)

validId :: Gen Label
validId = T.pack <$> validId'
    where validId' = (:) <$> elements ['a'..'z']
                         <*> listOf (elements $ ['a'..'z'] ++
                                                ['A'..'Z'] ++
                                                ['0'..'9'])
