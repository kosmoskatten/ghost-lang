{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GhostLang.InterpreterM
    ( InterpreterM (..)
    , State
    , runInterpreter

      -- API towards ghost-lang interpretation. Counters:
    , incInstrInvoked
    , incLoopCmds
    , incConcCmds
    , incPatternRuns
    , incProcCalls

      -- Value evaluation:
    , evalValue

    , get
    , liftIO
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
import GHC.Int (Int64)
import GhostLang.Counter ( Counter (..) 
                         , incInstrInvoked'
                         , incLoopCmds'
                         , incConcCmds'
                         , incPatternRuns'
                         , incProcCalls'
                         )
import GhostLang.Scope (Scope, emptyScope)
import GhostLang.Types (Value (..))

-- | Interpreter monad type for interpretation of instructions
-- implementing the InstructionSet type class.
newtype InterpreterM a = 
    InterpreterM 
      { extractInterpreterM :: ReaderT Scope (StateT State IO) a }
  deriving (Functor, Applicative, Monad
           , MonadReader Scope, MonadState State, MonadIO)

-- | Type alias for the interpreter state.
type State = [TVar Counter]

-- | Run an InterpreterM action. The action will be supplied the given
-- counters and an empty reader set. Returned from the action are the
-- counters in exactly the same order as when supplied.
runInterpreter :: State -> InterpreterM () -> IO ()
runInterpreter counters interpreter =
  evalStateT (runReaderT (extractInterpreterM interpreter) emptyScope) counters

-- | Increase the counter for invoked instructions.
incInstrInvoked :: InterpreterM ()
incInstrInvoked = updateCounter incInstrInvoked'

-- | Increase counter for the number of loop commands.
incLoopCmds :: InterpreterM ()
incLoopCmds = updateCounter incLoopCmds'

-- | Increase the counter for the number of concurrently commands.
incConcCmds :: InterpreterM ()
incConcCmds = updateCounter incConcCmds'

-- | Increase the counter for executed patterns.
incPatternRuns :: Text -> InterpreterM ()
incPatternRuns name = updateCounter $ incPatternRuns' name

-- | Increase the counter for called procedures.
incProcCalls :: Text -> InterpreterM ()
incProcCalls name = updateCounter $ incProcCalls' name

-- | Evaluate a value.
evalValue :: Value -> InterpreterM Int64
evalValue (Literal x) = return x
evalValue _ = error "Not yet implemented"

updateCounter :: (Counter -> Counter) -> InterpreterM ()
updateCounter g = mapM_ (\tvar -> atomically' $ modifyTVar' tvar g) =<< get

atomically' :: STM a -> InterpreterM a
atomically' = liftIO . atomically
