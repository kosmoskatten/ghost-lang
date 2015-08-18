{-# LANGUAGE OverloadedStrings #-}
module GhostLang.Node
    ( runNode
    ) where

import Control.Concurrent.STM (TVar)
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import GhostLang (compileAndLink)
import GhostLang.API ( LoadProgram (..)
                     , LoadProgramResult (..)
                     , FromJSON
                     , encode
                     , decode' )
import GhostLang.Node.State (State, emptyState)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Text.Printf (printf)
import qualified Data.ByteString.Lazy.Char8 as LBS

-- | Start and run the ghost-node.
runNode :: Int -> IO ()
runNode port = do
  state <- emptyState
  run port $ router state

-- | Route request to their handlers.
router :: State -> Application
router state request respond = 
    case pathInfo request of
      ["program", "load"]
          | requestMethod request == "POST" -> 
              handleProgramLoad state request respond
          | otherwise                       -> 
              handleNotAllowed "POST" request respond

      -- No matching handler is found.
      _ -> notFound request respond

handleProgramLoad :: State -> Application
handleProgramLoad state request respond = do
  msg <- decodeBody request
  printf "Got path: %s\n" (filePath msg)
  result <- compileAndLink (filePath msg)
  case result of
    Right prog -> do
        let answer = LoadProgramResult { progId = "/hej/hopp" }
        respond $ responseLBS status201 [("Content-Type", "application/json")] $
                              encode answer
    Left err   ->
        respond $ responseLBS status409 [("Content-Type", "text/plain")] $
                LBS.pack err

handleNotAllowed :: ByteString -> Application
handleNotAllowed allow _ respond = 
    respond $ responseLBS status405 [("Allow", allow)] ""

notFound :: Application
notFound _ respond = 
    respond $ responseLBS status404 [("Content-Type", "text/plain")] "Not Found"

decodeBody :: FromJSON a => Request -> IO a
decodeBody req = fromJust . decode' <$> lazyRequestBody req
  
