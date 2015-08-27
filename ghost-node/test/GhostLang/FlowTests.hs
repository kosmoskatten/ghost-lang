module GhostLang.FlowTests
    ( initStateTest
    , setHttpConfigTest
    ) where

import GhostLang (emptyNetworkConfiguration)
import GhostLang.API (Service (..))
import GhostLang.Node.Flow ( getHttpConfig
                           , setHttpConfig
                           , listPrograms
                           )
import GhostLang.Node.State ( NetworkConfiguration (..)
                            , initState 
                            )
import Test.HUnit

-- | Test some stuff about the initial state.
initStateTest :: Assertion
initStateTest = do
  state      <- initState

  -- Http configuration shall be equal to what's given by the empty
  -- network configuration.
  initConfig <- getHttpConfig state
  emptyService @=? initConfig

  -- There shall be no programs in the initial state.
  initPrograms <- listPrograms state
  [] @=? initPrograms

-- | Test that the setting of a new http
-- configuration is working as intended.
setHttpConfigTest :: Assertion
setHttpConfigTest = do
  state      <- initState
               
  let newHttp = Service { serviceAddress = "http://localhost"
                        , servicePort    = 8080 }
  setHttpConfig state newHttp
  newConfig <- getHttpConfig state
  newHttp @=? newConfig

emptyService :: Service
emptyService =
    Service { serviceAddress = httpServiceAddress emptyNetworkConfiguration
            , servicePort    = httpServicePort emptyNetworkConfiguration }
