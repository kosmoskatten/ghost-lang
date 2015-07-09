{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs         #-}
module GhostLang.Intrinsic
    ( IntrinsicSet (..)
    ) where

import GHC.Generics (Generic)
import GhostLang.Interpreter (InstructionSet (..))
import GhostLang.InterpreterM (InterpreterM)
import GhostLang.Types (Value)

-- | The data type of the intrinsic operations of ghost-lang.
data IntrinsicSet where
    Sleep :: Value -> IntrinsicSet
    deriving (Eq, Generic, Show)

