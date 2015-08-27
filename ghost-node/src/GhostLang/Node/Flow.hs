{-# LANGUAGE RecordWildCards #-}
module GhostLang.Node.Flow
    ( getHttpConfig
    , setHttpConfig
    ) where

import Control.Concurrent.STM ( TVar
                              , atomically
                              , newTVarIO
                              , modifyTVar'
                              , readTVarIO
                              )
import GhostLang.API (Service (..))
import GhostLang.Node.State ( State (..)
                            , NetworkConfiguration (..)
                            , modifyTVar'IO
                            )
-- | Get the current http configuration.
getHttpConfig :: State -> IO Service
getHttpConfig state = do
  nw <- readTVarIO $ networkConf state
  return $ Service { serviceAddress = httpServiceAddress nw
                   , servicePort    = httpServicePort nw }

-- | Set the http configuration.
setHttpConfig :: State -> Service -> IO ()
setHttpConfig State {..} Service {..} = 
    modifyTVar'IO networkConf $ \nw -> 
        nw { httpServiceAddress = serviceAddress
           , httpServicePort    = servicePort }
