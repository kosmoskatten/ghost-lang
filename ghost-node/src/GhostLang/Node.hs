{-# LANGUAGE OverloadedStrings #-}
module GhostLang.Node
    ( runNode
    ) where

import Data.ByteString (ByteString)
import GhostLang.API ( LoadProgram (..)
                     , LoadProgramResult (..)
                     , encode
                     , decode )
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)

-- | Start and run the ghost-node.
runNode :: Int -> IO ()
runNode port = run port router

-- | Route request to their handlers.
router :: Application
router request respond = respond $ 
    case pathInfo request of
      ["program", "load"]
          | requestMethod request == "POST" -> handleProgramLoad request
          | otherwise                       -> handleNotAllowed "POST"

      -- No matching handler is found.
      _ -> notFound

handleProgramLoad :: Request -> Response
handleProgramLoad _ = 
    responseLBS status409 [("Content-Type", "application/json")] $
                encode LoadProgramResult { description = Just "tjoho" }

handleNotAllowed :: ByteString -> Response
handleNotAllowed allow = responseLBS status405 [("Allow", allow)] ""

notFound :: Response
notFound = responseLBS status404 [("Content-Type", "text/plain")] "Not Found"
