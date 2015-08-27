module GhostLang.FlowTests
    ( setHttpConfigTest
    ) where

import GhostLang (emptyNetworkConfiguration)
import GhostLang.API (Service (..))
import GhostLang.Node.Flow ( getHttpConfig
                           , setHttpConfig
                           )
import GhostLang.Node.State ( NetworkConfiguration (..)
                            , initState 
                            )
import Test.HUnit

-- | Test that the initial configuration is equal to the empty network
-- configuration. Also test that the setting of a new http
-- configuration is working as intended.
setHttpConfigTest :: Assertion
setHttpConfigTest = do
  state      <- initState
  initConfig <- getHttpConfig state
  emptyService @=? initConfig
               
  let newHttp = Service { serviceAddress = "http://localhost"
                        , servicePort    = 8080 }
  setHttpConfig state newHttp
  newConfig <- getHttpConfig state
  newHttp @=? newConfig

emptyService :: Service
emptyService =
    Service { serviceAddress = httpServiceAddress emptyNetworkConfiguration
            , servicePort    = httpServicePort emptyNetworkConfiguration }
