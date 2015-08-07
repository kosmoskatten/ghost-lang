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
import GhostLang.Types (TimeUnit, Pace, Payload)
import Text.Printf (printf)

-- | Http method.
data Method = GET | POST | PUT
    deriving (Eq, Generic, Show)

-- | Content flags. High level descriptions for what become http
-- headers flags on both client and server sides.
data Content = Audio | Html | Image | M2M | Script | Video
    deriving (Eq, Generic, Show)

-- | The data type of the intrinsic operations of ghost-lang.
data IntrinsicSet where
    Delay :: !TimeUnit -> IntrinsicSet
    -- ^ Delay the executing thread for value milliseconds.

    Http :: !Method -> ![Content] -> !Payload -> !(Maybe Pace) -> IntrinsicSet
    -- ^ Perform the http operation.
    deriving (Eq, Generic, Show)

-- | Implementation of the InstructionSet type class.
instance InstructionSet IntrinsicSet where
    exec (Delay duration) = do
      duration' <- evalTimeUnit duration

      trace $ printf "Delay %d us" duration'
      whenChecked $ do
        liftIO $ threadDelay duration'
      

