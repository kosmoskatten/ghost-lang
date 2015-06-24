{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GhostLang.Interpreter
    ( Interpreter (..)
    ) where

import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.State (MonadState, StateT)
import Control.Monad.IO.Class (MonadIO)

newtype Interpreter a = 
    Interpreter { extractInterpreter :: StateT Int (ReaderT Int IO) a }
  deriving (Functor, Applicative, Monad
           , MonadReader Int, MonadState Int, MonadIO)
