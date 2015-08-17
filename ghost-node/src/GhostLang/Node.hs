{-# LANGUAGE OverloadedStrings #-}
module GhostLang.Node
    ( runNode
    ) where

import Control.Concurrent.STM (TVar)
import Data.ByteString (ByteString)
import GhostLang.API ( LoadProgram (..)
                     , LoadProgramResult (..)
                     , encode
                     , decode )
import GhostLang.Node.State (State, emptyState)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)

-- | Start and run the ghost-node.
runNode :: Int -> IO ()
runNode port = run port router

-- | Route request to their handlers.
router :: Application
router request respond = 
    case pathInfo request of
      ["program", "load"]
          | requestMethod request == "POST" -> 
              handleProgramLoad request respond
          | otherwise                       -> 
              handleNotAllowed "POST" request respond

      -- No matching handler is found.
      _ -> notFound request respond

handleProgramLoad :: Application
handleProgramLoad _ respond = undefined
--    respond $ responseLBS status409 [("Content-Type", "application/json")] $
--                  encode LoadProgramResult { description = Just "tjoho" }

handleNotAllowed :: ByteString -> Application
handleNotAllowed allow _ respond = 
    respond $ responseLBS status405 [("Allow", allow)] ""

notFound :: Application
notFound _ respond = 
    respond $ responseLBS status404 [("Content-Type", "text/plain")] "Not Found"
