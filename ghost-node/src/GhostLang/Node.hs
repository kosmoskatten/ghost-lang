{-# LANGUAGE OverloadedStrings #-}
module GhostLang.Node
    ( runNode
    ) where

import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import GhostLang (compileAndLink, toPatternList)
import GhostLang.API ( LoadProgram (..)
                     , Resource (..)
                     , FromJSON
                     , encode
                     , decode' )
import GhostLang.Node.IdGen (genId)
import GhostLang.Node.State ( State
                            , ProgramRepr (..)
                            , emptyState
                            , insertProgram )
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Text.Printf (printf)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T

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
  msg    <- decodeBody request
  result <- compileAndLink (T.unpack $ filePath msg)
  case result of
    Right prog -> do
        id' <- genId
        let res      = "/program/" `mappend` id'
            answer   = Resource { resourceId = res }
            progRepr = ProgramRepr { filePath_    = filePath msg
                                   , resourceId_  = res
                                   , ghostProgram = prog
                                   , patternList  = toPatternList prog
                                   }
        insertProgram state id' progRepr
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
  
