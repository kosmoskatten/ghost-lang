{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs         #-}
module GhostLang.Intrinsic
    ( IntrinsicSet (..)
    ) where

import Control.Concurrent (threadDelay)
import GHC.Generics (Generic)
import GhostLang.InstructionSet (InstructionSet (..))
import GhostLang.InterpreterM (evalTimeUnit, liftIO)
import GhostLang.Types (TimeUnit)

-- | The data type of the intrinsic operations of ghost-lang.
data IntrinsicSet where
    Delay :: TimeUnit -> IntrinsicSet
    -- ^ Delay the executing thread for value milliseconds.
    deriving (Eq, Generic, Show)

-- | Implementation of the InstructionSet type class.
instance InstructionSet IntrinsicSet where
    exec (Delay duration) = do
      duration' <- evalTimeUnit duration
      liftIO $ threadDelay duration'
      

