{-# LANGUAGE RecordWildCards #-}
module GhostLang.RuntimeState.Counter
    ( Counter (..)
    , HttpStatus (..)
    , emptyCounter
    , incPatternExecTime'
    , incInstrInvoked'
    , incLoopCmds'
    , incConcCmds'
    , incProcCalls'
    , updHttpGETCounters'
    , updHttpPUTCounters'
    , getProcCalls
    , getTotalProcCalls
    , incPatternRuns'
    , getPatternRuns
    , getTotalPatternRuns
    ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import GHC.Int (Int64)
import qualified Data.Map.Strict as Map

data HttpStatus = Success | Failure
   deriving (Eq, Show)

-- | Statistics counter for Ghost lang code execution.
data Counter =
    Counter { patternExecTime :: !NominalDiffTime
            -- ^ The total execution time in seconds.

            , instrInvoked    :: {-# UNPACK #-} !Int64 
            -- ^ The number of instructions invoked during the
            -- execution.

            , loopCmds        :: {-# UNPACK #-} !Int64
            -- ^ The number of loop commands run during the execution.

            , concCmds        :: {-# UNPACK #-} !Int64

            , procCalls       :: !(Map.Map Text Int64)
            -- ^ Book keeping of the number of times certain
            -- procedures are called.

            , patternRuns     :: !(Map.Map Text Int64)
            -- ^ Book keeping of the number of times certain patterns
            -- are executed.

            , httpGETExecTime :: !NominalDiffTime
            , httpGETBytes    :: {-# UNPACK #-} !Int64
            , httpGETSuccess  :: {-# UNPACK #-} !Int64
            , httpGETFailures :: {-# UNPACK #-} !Int64
            -- ^ Counters for http GET.

            , httpPUTExecTime :: !NominalDiffTime
            , httpPUTBytes    :: {-# UNPACK #-} !Int64
            , httpPUTSuccess  :: {-# UNPACK #-} !Int64
            , httpPUTFailures :: {-# UNPACK #-} !Int64
            -- ^ Counters for http PUT.
            }
    deriving Show

-- | Create an empty counter.
emptyCounter :: Counter
emptyCounter =
    Counter { patternExecTime = toEnum 0
            , instrInvoked    = 0
            , loopCmds        = 0
            , concCmds        = 0
            , procCalls       = Map.empty
            , patternRuns     = Map.empty
            , httpGETExecTime = toEnum 0
            , httpGETBytes    = 0
            , httpGETSuccess  = 0
            , httpGETFailures = 0
            , httpPUTExecTime = toEnum 0
            , httpPUTBytes    = 0
            , httpPUTSuccess  = 0
            , httpPUTFailures = 0
            }

-- | Increase the pattern execution time.
incPatternExecTime' :: NominalDiffTime -> Counter -> Counter
incPatternExecTime' d c@Counter {..} = 
    c { patternExecTime = patternExecTime + d }

-- | Increase the invoke counter by 1.
incInstrInvoked' :: Counter -> Counter
incInstrInvoked' c@Counter {..} = c { instrInvoked = instrInvoked + 1 }

-- | Increase the loop command counter by 1.
incLoopCmds' :: Counter -> Counter
incLoopCmds' c@Counter {..} = c { loopCmds = loopCmds + 1 }

-- | Increase the concurrently command counter by 1.
incConcCmds' :: Counter -> Counter
incConcCmds' c@Counter {..} = c { concCmds = concCmds + 1 }

-- | Increase the procedure call counter for procedure 'p' by one.
incProcCalls' :: Text -> Counter -> Counter
incProcCalls' p c@Counter {..} =
    let g = maybe (Just 1) (Just . (1 +))
    in c { procCalls = Map.alter g p procCalls }

-- | Update counters for http GET
updHttpGETCounters' :: NominalDiffTime -> Int64 
                    -> HttpStatus -> Counter -> Counter
updHttpGETCounters' d bytes status c@Counter {..} =
    let incSuccCount = if status == Success then 1 else 0
        incFailCount = if status == Failure then 1 else 0
    in c { httpGETExecTime = httpGETExecTime + d
         , httpGETBytes    = httpGETBytes    + bytes
         , httpGETSuccess  = httpGETSuccess  + incSuccCount
         , httpGETFailures = httpGETFailures + incFailCount
         }

-- | Update counters for http PUT.
updHttpPUTCounters' :: NominalDiffTime -> Int64
                    -> HttpStatus -> Counter -> Counter
updHttpPUTCounters' d bytes status c@Counter {..} =
    let incSuccCount = if status == Success then 1 else 0
        incFailCount = if status == Failure then 1 else 0
    in c { httpPUTExecTime = httpPUTExecTime + d
         , httpPUTBytes    = httpPUTBytes    + bytes
         , httpPUTSuccess  = httpPUTSuccess  + incSuccCount
         , httpPUTFailures = httpPUTFailures + incFailCount
         }    

-- | Get the value for the procedure call counter for procedure 'p'.
getProcCalls :: Text -> Counter -> Int64
getProcCalls p Counter {..} = fromMaybe 0 (Map.lookup p procCalls)

-- | Get the total number of procedure calls.
getTotalProcCalls :: Counter -> Int64
getTotalProcCalls Counter {..} = Map.foldl' (+) 0 procCalls

-- | Increase the pattern run counter for pattern 'p' by one.
incPatternRuns' :: Text -> Counter -> Counter
incPatternRuns' p c@Counter {..} = 
    let g = maybe (Just 1) (Just . (1 +))
    in c { patternRuns = Map.alter g p patternRuns }

-- | Get the value for the pattern run counter for pattern 'p'.
getPatternRuns :: Text -> Counter -> Int64
getPatternRuns p Counter {..} = fromMaybe 0 (Map.lookup p patternRuns)

-- | Get the total number of pattern runs.
getTotalPatternRuns :: Counter -> Int64
getTotalPatternRuns Counter {..} = Map.foldl' (+) 0 patternRuns
