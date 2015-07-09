{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs         #-}
module GhostLang.Intrinsic
    ( IntrinsicSet (..)
    ) where

import Control.Concurrent (threadDelay)
import GHC.Generics (Generic)
import GhostLang.Interpreter (InstructionSet (..))
import GhostLang.InterpreterM (evalValue, liftIO)
import GhostLang.Types (Value)

-- | The data type of the intrinsic operations of ghost-lang.
data IntrinsicSet where
    Delay :: Value -> IntrinsicSet
    -- ^ Delay the executing thread for value milliseconds.
    deriving (Eq, Generic, Show)

-- | Implementation of the InstructionSet type class.
instance InstructionSet IntrinsicSet where
    exec (Delay dur) = do
      dur' <- fromIntegral <$> evalValue dur
      liftIO $ threadDelay (dur' * 1000)
      

