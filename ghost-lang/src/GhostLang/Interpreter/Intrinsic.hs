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
import GhostLang.Interpreter.WebClient (httpGet, mkGetUrl)
import GhostLang.Types ( TimeUnit
                       , Pace
                       , Payload
                       , Method (..)
                       , Content
                       )
import Text.Printf (printf)

-- | The data type of the intrinsic operations of ghost-lang.
data IntrinsicSet where
    Delay :: !TimeUnit -> IntrinsicSet
    -- ^ Delay the executing thread for value milliseconds.

    Http :: !Method -> ![Content] -> !Payload -> !(Maybe Pace) -> IntrinsicSet
    -- ^ Perform the http operation.
    deriving (Eq, Generic, Show)

-- | Implementation of the InstructionSet type class.
instance InstructionSet IntrinsicSet where

    -- | Execute a delay command.
    exec (Delay duration) = do
      duration' <- evalTimeUnit duration

      trace $ printf "Delay %d us" duration'
      whenChecked $ do
        liftIO $ threadDelay duration'

    -- | Execute a http GET command.
    exec (Http GET _ payload _) = do
      url <- mkGetUrl payload

      trace $ printf "Http GET %s" url
--      whenChecked $ do
--        httpGet payload'

    exec (Http POST _ _ _) = undefined

    exec (Http PUT _ _ _) = undefined
