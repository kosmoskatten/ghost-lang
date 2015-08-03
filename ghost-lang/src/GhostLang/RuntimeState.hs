module GhostLang.RuntimeState
    ( TVar
    , RuntimeState (..)
    , NetworkConfiguration (..)
    , Mode (..)
    , emptyRuntimeState
    , emptyNetworkConfiguration
    , module GhostLang.RuntimeState.Counter
    ) where

import Control.Concurrent.STM (TVar)
import GhostLang.RuntimeState.Counter

data NetworkConfiguration = NetworkConfiguration

data Mode = Normal | Trace | Dry

data RuntimeState =
    RuntimeState { counters             :: ![TVar Counter]
                 , networkConfiguration :: !NetworkConfiguration
                 , mode                 :: !Mode
                 }

emptyRuntimeState :: RuntimeState
emptyRuntimeState = 
    RuntimeState { counters             = []
                 , networkConfiguration = emptyNetworkConfiguration
                 , mode                 = Normal }

emptyNetworkConfiguration :: NetworkConfiguration
emptyNetworkConfiguration = NetworkConfiguration
