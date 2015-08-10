{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GhostLang.Interpreter.InterpreterM
    ( InterpreterM (..)
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

    -- Payload evaluation:
    , evalPayload

    -- Checked execution of an action:
    , whenChecked

    -- Trace a message:
    , trace

    , ask
    , get
    , local
    , liftIO
    ) where

import Control.Concurrent (myThreadId)
import Control.Concurrent.STM ( STM
                              , atomically
                              , modifyTVar'
                              )
import Control.Monad (when)
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
import Data.Time (getCurrentTime)
import GHC.Int (Int64)
import GhostLang.Interpreter.Scope (Scope, emptyScope, lookup)
import GhostLang.RuntimeState ( Counter (..)
                              , Mode (..) 
                              , RuntimeState (..)
                              , defaultRuntimeState
                              , incInstrInvoked'
                              , incLoopCmds'
                              , incConcCmds'
                              , incPatternRuns'
                              , incProcCalls'
                              )
import GhostLang.Types ( Value (..)
                       , TimeUnit (..)
                       , Payload (..) )
import Prelude hiding (lookup)
import Text.Printf (printf)

-- | Interpreter monad type for interpretation of instructions
-- implementing the InstructionSet type class.
newtype InterpreterM a = 
    InterpreterM 
      { extractInterpreterM :: ReaderT Scope (StateT RuntimeState IO) a }
  deriving (Functor, Applicative, Monad
           , MonadReader Scope, MonadState RuntimeState, MonadIO)

-- | Run an InterpreterM action. The action will be supplied a runtime
-- state and an empty scope.
runInterpreter :: RuntimeState -> InterpreterM () -> IO ()
runInterpreter state interpreter =
  evalStateT (runReaderT (extractInterpreterM interpreter) emptyScope) state

-- | Run an interpreterM action for testing purposes. Run with an
-- empty counter set beside the setting of
runInterpreterTest :: Mode -> InterpreterM a -> IO a
runInterpreterTest mode' interpreter = do
  state <- defaultRuntimeState
  let state' = state { mode = mode' }
  evalStateT (runReaderT (extractInterpreterM interpreter) emptyScope) state'

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

-- | Evaluate a payload value to the number of bytes.
evalPayload :: Payload -> InterpreterM Int64
evalPayload (B v)  = evalValue v
evalPayload (KB v) = (1000 *) <$> evalValue v
evalPayload (MB v) = (1000000 *) <$> evalValue v
evalPayload (GB v) = (1000000000 *) <$> evalValue v

-- | Run the provided action iff the runtime state indicate execution.
whenChecked :: InterpreterM () -> InterpreterM ()
whenChecked act = do
  mode' <- mode <$> get
  when (mode' == Normal || mode' == Trace) act

-- | Trace a message.
trace :: String -> InterpreterM ()
trace msg = do
  mode' <- mode <$> get
  when (mode' == Trace) $ do
      tid <- liftIO myThreadId
      now <- liftIO getCurrentTime
      liftIO $ printf "%s - %s: %s\n" (show tid) (show now) msg
  when (mode' == Dry) $ do
      liftIO $ putStrLn msg

updateCounter :: (Counter -> Counter) -> InterpreterM ()
updateCounter g = do
  counters' <- counters <$> get
  mapM_ (\tvar -> atomically' $ modifyTVar' tvar g) counters'

atomically' :: STM a -> InterpreterM a
atomically' = liftIO . atomically
