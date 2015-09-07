{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GhostLang.Interpreter.InterpreterM
    ( InterpreterM (..)
    , runInterpreter
    , runInterpreterScoped
    , runInterpreterTest

      -- API towards ghost-lang interpretation. Counters:
    , incPatternExecTime
    , incInstrInvoked
    , incLoopCmds
    , incConcCmds
    , incPatternRuns
    , incProcCalls
    , updHttpGETCounters
    , updHttpPUTCounters

    -- Measure the time of an action:
    , timedAction

      -- Value evaluation:
    , evalValue

      -- TimeUnit evaluation:
    , evalTimeUnit

    -- Payload and Pace evaluation:
    , evalPayload
    , evalPace

    -- Trace a message:
    , logString

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
import Data.Time ( NominalDiffTime
                 , diffUTCTime
                 , getCurrentTime )
import GHC.Int (Int64)
import GhostLang.Interpreter.Scope (Scope, emptyScope, lookup)
import GhostLang.RuntimeState ( Counter (..)
                              , HttpStatus
                              , RuntimeState (..)
                              , defaultRuntimeState
                              , incPatternExecTime'
                              , incInstrInvoked'
                              , incLoopCmds'
                              , incConcCmds'
                              , incPatternRuns'
                              , incProcCalls'
                              , updHttpGETCounters'
                              , updHttpPUTCounters'
                              )
import GhostLang.Types ( Value (..)
                       , TimeUnit (..)
                       , Payload (..)
                       , Pace (..) )
import Prelude hiding (lookup)
import Text.Printf (printf)
import qualified GhostLang.GLog as Logger

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
runInterpreter = runInterpreterScoped emptyScope

-- | Run the interpreter with explicit scope. Necessary for concurrent
-- sections.
runInterpreterScoped :: Scope -> RuntimeState -> InterpreterM () -> IO ()
runInterpreterScoped scope state interpreter =
  evalStateT (runReaderT (extractInterpreterM interpreter) scope) state

-- | Run an interpreterM action for testing purposes. Run with an
-- empty counter set beside the setting of
runInterpreterTest :: InterpreterM a -> IO a
runInterpreterTest interpreter = do
  state <- defaultRuntimeState
  evalStateT (runReaderT (extractInterpreterM interpreter) emptyScope) state

-- | Increase the counter for pattern execution time.
incPatternExecTime :: NominalDiffTime -> InterpreterM ()
incPatternExecTime d = updateCounter $ incPatternExecTime' d

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

-- | Update the counters for http GET.
updHttpGETCounters :: NominalDiffTime -> Int64 -> HttpStatus -> InterpreterM ()
updHttpGETCounters d bytes status =
    updateCounter $ updHttpGETCounters' d bytes status

-- | Update the counters for http PUT.
updHttpPUTCounters :: NominalDiffTime -> Int64 -> HttpStatus -> InterpreterM ()
updHttpPUTCounters d bytes status =
    updateCounter $ updHttpPUTCounters' d bytes status

-- | Measure the time of an action.
timedAction :: InterpreterM a -> InterpreterM (a, NominalDiffTime)
timedAction act = do
  start  <- liftIO getCurrentTime
  result <- act
  stop   <- liftIO getCurrentTime
  return $! (result, stop `diffUTCTime` start)

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
evalTimeUnit (USec v) = fromIntegral               <$> evalValue v
evalTimeUnit (MSec v) = (1000 *) . fromIntegral    <$> evalValue v
evalTimeUnit (Sec  v) = (1000000 *) . fromIntegral <$> evalValue v

-- | Evaluate a payload value to the number of bytes.
evalPayload :: Payload -> InterpreterM Int64
evalPayload (B v)  = evalValue v
evalPayload (KB v) = (1000 *)       <$> evalValue v
evalPayload (MB v) = (1000000 *)    <$> evalValue v
evalPayload (GB v) = (1000000000 *) <$> evalValue v

-- | Evaluate a pace value to a corresponding byte value.
evalPace :: Pace -> InterpreterM Int64
evalPace (Bps v)  = bitsToBytes                  <$> evalValue v
evalPace (Kbps v) = bitsToBytes . (1000 *)       <$> evalValue v
evalPace (Mbps v) = bitsToBytes . (1000000 *)    <$> evalValue v
evalPace (Gbps v) = bitsToBytes . (1000000000 *) <$> evalValue v

bitsToBytes :: Int64 -> Int64
bitsToBytes n = n `div` 8

-- | Log a string if tracing is active.
logString :: String -> InterpreterM ()
logString str = do
  shallTrace' <- shallTrace <$> get
  when (shallTrace') $ do
    logger' <- logger <$> get
    liftIO $ Logger.logString logger' str

updateCounter :: (Counter -> Counter) -> InterpreterM ()
updateCounter g = do
  counters' <- counters <$> get
  mapM_ (\tvar -> atomically' $ modifyTVar' tvar g) counters'

atomically' :: STM a -> InterpreterM a
atomically' = liftIO . atomically
