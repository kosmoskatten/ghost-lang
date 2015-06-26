module GhostLang.Counter
    ( Counter (..)
    , emptyCounter
    ) where

import Data.Text (Text)
import GHC.Int (Int64)

-- | Statistics counter for Ghost lang code execution.
data Counter = 
    Counter { instrInvoked :: {-# UNPACK #-} !Int64 
            -- ^ The number of instructions invoked during the
            -- execution.
            }
    deriving Show

-- | Create an empty counter.
emptyCounter :: Counter
emptyCounter =
    Counter { instrInvoked = 0 }
