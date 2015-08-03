{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GhostLang.Interpreter.InterpreterM
    ( InterpreterM (..)
    , State
    , runInterpreter
    , runInterpreterTest

      -- API towards ghost-lang interpretation. Counters:
    , incInstrInvoked
    , incLoopCmds
    , incConcCmds
    , incPatternRuns
    , incProcCalls

      -- Value evaluation:
    , evalValue

      -- TimeUnit evaluation:
    , evalTimeUnit

    , ask
    , get
    , local
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
                            , ask
                            , local
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
import GhostLang.Interpreter.Scope (Scope, emptyScope, lookup)
import GhostLang.Types (Value (..), TimeUnit (..))
import Prelude hiding (lookup)

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
-- counters and an empty reader set.
runInterpreter :: State -> InterpreterM () -> IO ()
runInterpreter counters interpreter =
  evalStateT (runReaderT (extractInterpreterM interpreter) emptyScope) counters

-- | Run an interpreterM action for testing purposes. Run with an
-- empty counter set.
runInterpreterTest :: InterpreterM a -> IO a
runInterpreterTest interpreter =
    evalStateT (runReaderT (extractInterpreterM interpreter) emptyScope) []

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
evalValue (Stored x) = do
  v <- lookup x <$> ask
  case v of
    Just v' -> evalValue v'
    _       -> error $ "Cannot lookup stored value: " ++ show x
  
evalValue _ = error "Not yet implemented"

-- | Evaluate a TimeUnit. The result is a value suitable for feeding
-- threadDelay.
evalTimeUnit :: TimeUnit -> InterpreterM Int
evalTimeUnit (USec v) = fromIntegral <$> evalValue v
evalTimeUnit (MSec v) = (1000 *) . fromIntegral <$> evalValue v
evalTimeUnit (Sec  v) = (1000000 *) . fromIntegral <$> evalValue v

updateCounter :: (Counter -> Counter) -> InterpreterM ()
updateCounter g = mapM_ (\tvar -> atomically' $ modifyTVar' tvar g) =<< get

atomically' :: STM a -> InterpreterM a
atomically' = liftIO . atomically
