-- | Collection of state information for the execution of a pattern.
module GhostLang.RuntimeState
    ( TVar
    , RuntimeState (..)
    , NetworkConfiguration (..)
    , GLog
    , defaultRuntimeState
    , emptyNetworkConfiguration
    , module GhostLang.RuntimeState.Counter
    ) where

import Control.Concurrent.STM (TVar)
import GhostLang.Conduit (DataChunk, genDataChunk)
import GhostLang.GLog (GLog, newEmptyGLog)
import GhostLang.RuntimeState.Counter
import Network.HTTP.Client ( Manager
                           , defaultManagerSettings
                           , newManager
                           )

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

                 , dataChunk            :: !DataChunk
                 -- ^ A data chunk used for upload payload data
                 -- generation.

                 , shallTrace           :: !Bool
                 -- ^ The execution mode.

                 , logger               :: !GLog
                 -- ^ The logger.
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

-- | Create a default setup runtime state. NOTE: Should only be used by test
-- codes!!!
defaultRuntimeState :: IO RuntimeState
defaultRuntimeState = do
  mgr  <- newManager defaultManagerSettings
  dc   <- genDataChunk 128 -- Just some small size.
  glog <- newEmptyGLog
  return $ RuntimeState 
             { counters             = []
             , networkConfiguration = emptyNetworkConfiguration
             , connectionMgr        = mgr
             , dataChunk            = dc
             , shallTrace           = False
             , logger               = glog }

emptyNetworkConfiguration :: NetworkConfiguration
emptyNetworkConfiguration = 
    NetworkConfiguration { httpServiceAddress = "-"
                         , httpServicePort    = 0
                         , srcIpAddress       = Nothing
                         }
