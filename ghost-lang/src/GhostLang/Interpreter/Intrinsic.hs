{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs         #-}
module GhostLang.Interpreter.Intrinsic
    ( IntrinsicSet (..)
    ) where

import Control.Concurrent (threadDelay)
import GHC.Generics (Generic)
import GhostLang.Interpreter.InstructionSet (InstructionSet (..))
import GhostLang.Interpreter.InterpreterM ( evalTimeUnit
                                          , trace
                                          , whenChecked
                                          , liftIO )
import GhostLang.Types (TimeUnit)
import Text.Printf (printf)

-- | The data type of the intrinsic operations of ghost-lang.
data IntrinsicSet where
    Delay :: TimeUnit -> IntrinsicSet
    -- ^ Delay the executing thread for value milliseconds.
    deriving (Eq, Generic, Show)

-- | Implementation of the InstructionSet type class.
instance InstructionSet IntrinsicSet where
    exec (Delay duration) = do
      duration' <- evalTimeUnit duration

      trace $ printf "Delay %d us" duration'
      whenChecked $ do
        liftIO $ threadDelay duration'
      

