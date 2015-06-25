{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GhostLang.Interpreter
    ( Interpreter (..)
    , Executable (..)
    , Operation (..)
    , Pattern (..)
    , Procedure (..)
    , execPattern
    , runInterpreter
    ) where

import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)

newtype Interpreter a = 
    Interpreter { extractInterpreter :: ReaderT Int (StateT Int IO) a }
  deriving (Functor, Applicative, Monad
           , MonadReader Int, MonadState Int, MonadIO)

class Executable a where
    exec :: a -> Interpreter ()

data Pattern a where
    Pattern :: Executable a => !Text -> !Int -> ![Operation a] -> Pattern a

data Procedure a where
    Procedure :: Executable a => !Text -> ![Operation a] -> Procedure a

data Operation a where
    Invoke :: Executable a => !a -> Operation a
    -- ^ Invoke is the operation of invoking a simple element of the
    -- ghost language. E.g. invoking a http get.

    Call :: Executable a => Procedure a -> Operation a
    -- ^ Call is the operation of calling a procedure. The procedure
    -- is given its arguments in a local context of the Interpreter
    -- monad.

    Unresolved :: Executable a => !Text -> Operation a

-- | Run the interpreter on the provided pattern.
runInterpreter :: Executable a => 
                  (Pattern a -> Interpreter ()) -> Pattern a -> IO ()
runInterpreter interpreter pattern =
  evalStateT (runReaderT (extractInterpreter (interpreter pattern)) 0) 0

execPattern :: Executable a => Pattern a -> Interpreter ()
execPattern (Pattern _ _ ops) = mapM_ exec ops

instance Executable a => Executable (Operation a) where
    exec (Invoke instr) = exec instr
