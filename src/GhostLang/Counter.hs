{-# LANGUAGE RecordWildCards #-}
module GhostLang.Counter
    ( Counter (..)
    , emptyCounter
    , incInstrInvoked'
    , incLoopCmds'
    , incConcCmds'
    , incProcCalls'
    , getTotalProcCalls
    , incPatternRuns'
    , getPatternRuns
    , getTotalPatternRuns
    ) where

import Data.Text (Text)
import GHC.Int (Int64)
import qualified Data.Map.Strict as Map

-- | Statistics counter for Ghost lang code execution.
data Counter = 
    Counter { instrInvoked :: {-# UNPACK #-} !Int64 
            -- ^ The number of instructions invoked during the
            -- execution.

            , loopCmds     :: {-# UNPACK #-} !Int64
            -- ^ The number of loop commands run during the execution.

            , concCmds     :: {-# UNPACK #-} !Int64

            , procCalls    :: !(Map.Map Text Int64)
            -- ^ Book keeping of the number of times certain
            -- procedures are called.

            , patternRuns  :: !(Map.Map Text Int64)
            -- ^ Book keeping of the number of times certain patterns
            -- are executed.
            }
    deriving Show

-- | Create an empty counter.
emptyCounter :: Counter
emptyCounter =
    Counter { instrInvoked  = 0
            , loopCmds      = 0
            , concCmds      = 0
            , procCalls     = Map.empty
            , patternRuns   = Map.empty
            }

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
getPatternRuns p Counter {..} = maybe 0 id (Map.lookup p patternRuns)

-- | Get the total number of pattern runs.
getTotalPatternRuns :: Counter -> Int64
getTotalPatternRuns Counter {..} = Map.foldl' (+) 0 patternRuns
