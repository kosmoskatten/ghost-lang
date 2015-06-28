{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GhostLang.InterpreterM
    ( InterpreterM (..)
    , State
    , runInterpreter

      -- API towards ghost-lang interpreter
    , incInstrInvoked
    , incPatternRuns
    , incProcCalls
    ) where

import Control.Concurrent.STM ( STM
                              , TVar
                              , atomically
                              , modifyTVar'
                              )
import Control.Monad.Reader ( MonadReader
                            , ReaderT
                            , runReaderT
                            )
import Control.Monad.State ( MonadState
                           , StateT
                           , evalStateT
                           , get
                           )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import GhostLang.Counter ( Counter (..) 
                         , incInstrInvoked'
                         , incPatternRuns'
                         , incProcCalls'
                         )

-- | Interpreter monad type for interpretation of instructions
-- implementing the InstructionSet type class.
newtype InterpreterM a = 
    InterpreterM 
      { extractInterpreterM :: ReaderT Int (StateT State IO) a }
  deriving (Functor, Applicative, Monad
           , MonadReader Int, MonadState State, MonadIO)

-- | Type alias for the interpreter state.
type State = [TVar Counter]

-- | Run an InterpreterM action. The action will be supplied the given
-- counters and an empty reader set. Returned from the action are the
-- counters in exactly the same order as when supplied.
runInterpreter :: State -> InterpreterM () -> IO ()
runInterpreter counters interpreter =
  evalStateT (runReaderT (extractInterpreterM interpreter) 0) counters

-- | Increase the counter for invoked instructions.
incInstrInvoked :: InterpreterM ()
incInstrInvoked = updateCounter incInstrInvoked'

-- | Increase the counter for executed patterns.
incPatternRuns :: Text -> InterpreterM ()
incPatternRuns name = updateCounter $ incPatternRuns' name

-- | Increase the counter for called procedures.
incProcCalls :: Text -> InterpreterM ()
incProcCalls name = updateCounter $ incProcCalls' name

updateCounter :: (Counter -> Counter) -> InterpreterM ()
updateCounter g = mapM_ (\tvar -> atomically' $ modifyTVar' tvar g) =<< get

atomically' :: STM a -> InterpreterM a
atomically' = liftIO . atomically
