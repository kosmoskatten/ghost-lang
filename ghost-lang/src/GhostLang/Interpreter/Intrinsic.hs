{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs         #-}
module GhostLang.Interpreter.Intrinsic
    ( IntrinsicSet (..)
    ) where

import Control.Concurrent (threadDelay)
import GHC.Generics (Generic)
import GhostLang.Interpreter.InstructionSet (InstructionSet (..))
import GhostLang.Interpreter.InterpreterM ( InterpreterM
                                          , get
                                          , evalTimeUnit
                                          , evalPayload
                                          , evalPace
                                          , logString
                                          , updHttpGETCounters
                                          , updHttpPUTCounters
                                          , timedAction
                                          , liftIO )
import GhostLang.Interpreter.WebClient (httpGet, httpPut)
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

      logString $ printf "Delay %d us" duration'
      liftIO $ threadDelay duration'

    -- | Execute a http GET command.
    exec (Http GET _ payload pace) = do
      url   <- mkGetUrl payload
      mgr   <- connectionMgr <$> get
      pace' <- maybePace pace

      logString $ printf "Http GET %s" url
      ((status, bytes), d) <- timedAction $ liftIO $ httpGet mgr url pace'
      updHttpGETCounters d bytes status

    -- | Execute a http PUT command.
    exec (Http PUT _ payload pace) = do
      url <- mkPutUrl pace
      mgr <- connectionMgr <$> get
      dc  <- dataChunk <$> get
      pl  <- fromIntegral <$> evalPayload payload

      logString $ printf "Http PUT %s" url
      ((status, bytes), d) <- timedAction $ liftIO $ httpPut mgr url dc pl
      updHttpPUTCounters d bytes status

    exec (Http POST _ _ _) = undefined

maybePace :: Maybe Pace -> InterpreterM (Maybe Int)
maybePace (Just pace) = Just . fromIntegral <$> evalPace pace
maybePace Nothing     = return Nothing

mkGetUrl :: Payload -> InterpreterM String
mkGetUrl payload = do
  size <- evalPayload payload
  nc   <- networkConfiguration <$> get
  return $! printf "%s:%d/download?size=%ld" (httpServiceAddress nc)
                                             (httpServicePort nc) size

mkPutUrl :: Maybe Pace -> InterpreterM String
mkPutUrl mpace = do
  nc   <- networkConfiguration <$> get
  pace <- maybePace mpace
  case pace of
    Just pace' ->
      return $! printf "%s:%d/upload?pace=%d" (httpServiceAddress nc)
                                              (httpServicePort nc) pace'
    Nothing    ->
      return $! printf "%s:%d/upload" (httpServiceAddress nc)
                                      (httpServicePort nc)
