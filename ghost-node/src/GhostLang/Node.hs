{-# LANGUAGE OverloadedStrings #-}
module GhostLang.Node
    ( runNode
    ) where

import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Text (Text)
import GhostLang (PatternTuple, compileAndLink, toPatternList)
import GhostLang.API ( LoadProgram (..)
                     , PatternInfo (..)
                     , Resource (..)
                     , FromJSON
                     , encode
                     , decode' )
import GhostLang.Node.IdGen (genId)
import GhostLang.Node.State ( State
                            , ProgramRepr (..)
                            , emptyState
                            , insertProgram
                            , lookupProgram )
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
      -- Route a request for compiling and loading a
      -- ghost-program. Only POST request are accepted.
      ["program", "load"]
          | requestMethod request == "POST" -> 
              handleProgramLoad state request respond
          | otherwise                       -> 
              handleNotAllowed "POST" request respond

      -- Route a request for listing all the patterns contained in a
      -- compiled ghost-program. Only GET requests are accepted.
      ["program", resId, "list"]
          | requestMethod request == "GET" ->
              handleProgramList state resId request respond
          | otherwise                      ->
              handleNotAllowed "GET" request respond

      -- No matching handler is found.
      _ -> notFound request respond

-- | Handle the request of compiling and loading a ghost-program. If
-- the compilation is successful, the compiled program is stored and a
-- response code 201 carrying a JSON with the resource id is
-- returned. Otherwise an error with code 409 is returned.
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
-- | List the patterns for the given program.
handleProgramList :: State -> Text -> Application
handleProgramList state resId request respond = do
  maybeProgram <- lookupProgram state resId
  case maybeProgram of
    Just prog -> do
      let answer = map (\(l, w, _) -> PatternInfo l w) $ patternList prog
      respond $ responseLBS status200 [("Content-Type", "application/json")] $
                            encode answer
    Nothing   -> notFound request respond
  
handleNotAllowed :: ByteString -> Application
handleNotAllowed allow _ respond = 
    respond $ responseLBS status405 [("Allow", allow)] ""

notFound :: Application
notFound _ respond = 
    respond $ responseLBS status404 [("Content-Type", "text/plain")] 
                "Resource Not Found"

decodeBody :: FromJSON a => Request -> IO a
decodeBody req = fromJust . decode' <$> lazyRequestBody req
  
