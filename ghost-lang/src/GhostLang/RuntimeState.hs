-- | Collection of state information for the execution of a pattern.
module GhostLang.RuntimeState
    ( TVar
    , RuntimeState (..)
    , NetworkConfiguration (..)
    , Mode (..)
    , defaultRuntimeState
    , emptyNetworkConfiguration
    , module GhostLang.RuntimeState.Counter
    ) where

import Control.Concurrent.STM (TVar)
import GhostLang.RuntimeState.Counter
import Network.HTTP.Client ( Manager
                           , defaultManagerSettings
                           , newManager
                           )

-- | Execution modes for pattern. In Normal mode no tracing is made,
-- Trace mode executes all actions with trace activated and Dry mode
-- only perform tracing.
data Mode = Normal | Trace | Dry
    deriving (Eq, Show)

-- | RuntimState record. Each pattern instance is given each its own
-- record instance.
data RuntimeState =
    RuntimeState { counters             :: ![TVar Counter]
                 -- ^ A list of shared counters (shared between
                 -- patterns/threads).

                 , networkConfiguration :: !NetworkConfiguration
                 -- ^ Network configuration parameters.                      

                 , connectionMgr        :: !Manager
                 -- ^ A connection manager local for this state
                 -- instance.

                 , mode                 :: !Mode
                 -- ^ The execution mode.
                 }

-- | Network configuration parameters.
data NetworkConfiguration = 
    NetworkConfiguration { httpServiceAddress :: !String
                         -- ^ The network address to the http service.

                         , httpServicePort    :: !Int
                         -- ^ The network port to the http service.

                         , srcIpAddress       :: !(Maybe String)
                         -- ^ The requested source IP address for the
                         -- connections in the flow (if any).
                         }

-- | Create a default setup runtime state.
defaultRuntimeState :: IO RuntimeState
defaultRuntimeState = do
  mgr <- newManager defaultManagerSettings
  return $ RuntimeState 
             { counters             = []
             , networkConfiguration = emptyNetworkConfiguration
             , connectionMgr        = mgr
             , mode                 = Normal }

emptyNetworkConfiguration :: NetworkConfiguration
emptyNetworkConfiguration = 
    NetworkConfiguration { httpServiceAddress = "-"
                         , httpServicePort    = 0
                         , srcIpAddress       = Nothing
                         }
