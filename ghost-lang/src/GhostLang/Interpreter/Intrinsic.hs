{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs         #-}
module GhostLang.Interpreter.Intrinsic
    ( IntrinsicSet (..)
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import GHC.Generics (Generic)
import GhostLang.Interpreter.InstructionSet (InstructionSet (..))
import GhostLang.Interpreter.InterpreterM ( InterpreterM
                                          , get
                                          , evalTimeUnit
                                          , evalPayload
                                          , evalPace
                                          , trace
                                          , whenChecked
                                          , liftIO )
import GhostLang.Interpreter.WebClient ( httpGet
                                       , eagerConsumer
                                       , pacedConsumer
                                       , initVirtualBuffer
                                       )
import GhostLang.RuntimeState ( RuntimeState (..)
                              , NetworkConfiguration (..)
                              )
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

    -- | Execute a http GET command. No paceing.
    exec (Http GET _ payload Nothing) = do
      url <- mkGetUrl payload

      trace $ printf "Http GET %s" url
      whenChecked $ do
        mgr <- connectionMgr <$> get
        void $ liftIO $ httpGet mgr url eagerConsumer

    -- | Execute a http GET command with paceing.
    exec (Http GET _ payload (Just pace)) = do
      url <- mkGetUrl payload
      pace' <- evalPace pace

      trace $ printf "Http GET %s, pace %ld bytes per sec" url pace'
      whenChecked $ do
        mgr    <- connectionMgr <$> get
        buffer <- liftIO $ initVirtualBuffer pace'
        void $ liftIO $ httpGet mgr url (pacedConsumer buffer)

    exec (Http POST _ _ _) = undefined

    exec (Http PUT _ _ _) = undefined

mkGetUrl :: Payload -> InterpreterM String
mkGetUrl payload = do
--  size <- evalPayload payload
  nc   <- networkConfiguration <$> get
--  return $! printf "%s:%d/download?size=%ld" (httpServiceAddress nc)
--                                             (httpServicePort nc) size
  return $ printf "%s:%d" (httpServiceAddress nc) (httpServicePort nc)
