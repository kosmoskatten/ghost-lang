{-# LANGUAGE OverloadedStrings #-}
module GhostLang.Node
    ( runNode
    ) where

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)

-- | Start and run the ghost-node.
runNode :: Int -> IO ()
runNode port = run port router

router :: Application
router _ respond = 
    respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hepp"
