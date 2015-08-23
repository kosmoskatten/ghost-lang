{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GhostLang.Node
    ( runNode
    ) where

import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Time (getCurrentTime)
import Data.Text (Text)
import GhostLang (compileAndLink, toPatternList)
import GhostLang.API ( ProgramPath (..)
                     , PatternInfo (..)
                     , Resource (..)
                     , FromJSON
                     , ToJSON
                     , encode
                     , decode' )
import GhostLang.Node.IdGen (genId)
import GhostLang.Node.State ( State (..)
                            , ProgramRepr (..)
                            , initState
                            , insertProgram
                            , lookupProgram
                            , allPrograms
                            , getHttpConf
                            , setHttpConf )
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Log.FastLogger (ToLogStr (..), pushLogStrLn)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T

-- | Start and run the ghost-node.
runNode :: Int -> IO ()
runNode port = do
  state <- initState
  logg state $ printf "Ghost Node listening on port %d" port
  run port $ router state

-- | Route request to their handlers.
router :: State -> Application
router state request respond = do
    logg state $ printf "%s %s" (BS.unpack $ requestMethod request) 
                                (BS.unpack $ rawPathInfo request)

    case pathInfo request of
      -- Request to read or set the http configuration.
      ["configuration", "http"]
          | requestMethod request == "GET"  -> 
              handleReadHttpConfig state request respond
          | requestMethod request == "PUT" ->
              handleSetHttpConfig state request respond
          | otherwise                       -> 
              handleNotAllowed ["GET", "PUT"] request respond

      -- Route a request for compiling and loading a
      -- ghost-program. Only POST request are accepted.
      ["program", "load"]
          | requestMethod request == "POST" -> 
              handleProgramLoad state request respond
          | otherwise                       -> 
              handleNotAllowed ["POST"] request respond

      -- Route a request for listing all the resource ids for the
      -- programs loaded.
      ["program", "list"]
          | requestMethod request == "GET" ->
              handleProgramList state request respond
          | otherwise                      ->
              handleNotAllowed ["GET"] request respond

      -- Route a request for listing all the patterns contained in a
      -- selected compiled ghost-program. Only GET requests are
      -- accepted.
      ["program", resId, "list"]
          | requestMethod request == "GET" ->
              handleSelectedProgramList state resId request respond
          | otherwise                      ->
              handleNotAllowed ["GET"] request respond

      -- No matching handler is found.
      _ -> notFound request respond

-- | Read the http configuration. Return the configuration as JSON
-- with response code 200.
handleReadHttpConfig :: State -> Application
handleReadHttpConfig state _ respond = do
  answer <- getHttpConf state
  respond $ jsonResponse status200 answer

-- | Set the http configuration carried in a Service record. Respond
-- with an empty response code 200.
handleSetHttpConfig :: State -> Application
handleSetHttpConfig state request respond = do
  msg <- decodeBody request
  setHttpConf state msg
  respond $ textResponse status200 ""

-- | Handle the request of compiling and loading a ghost-program. If
-- the compilation is successful, the compiled program is stored and a
-- response code 201 carrying a JSON with the resource id is
-- returned. Otherwise an error with code 409 is returned.
handleProgramLoad :: State -> Application
handleProgramLoad state request respond = do
  msg    <- decodeBody request
  result <- compileAndLink (T.unpack $ programPath msg)
  case result of
    Right prog -> do
        id' <- genId
        let resId    = "/program/" `mappend` id'
            answer   = Resource { resourceId = resId }
            progRepr = ProgramRepr { programPath_ = programPath msg
                                   , resourceId_  = resId
                                   , ghostProgram = prog
                                   , patternList  = toPatternList prog
                                   }
        insertProgram state id' progRepr
        respond $ jsonResponse status201 answer
    Left err   ->
        respond $ textResponse status409 $ LBS.pack err

-- | List the resource ids for all loaded programs.
handleProgramList :: State -> Application
handleProgramList state _ respond = do
  progs <- allPrograms state
  let answer = map (Resource . resourceId_) progs
  respond $ jsonResponse status200 answer

-- | List the patterns for the selected program.
handleSelectedProgramList :: State -> Text -> Application
handleSelectedProgramList state resId request respond = do
  maybeProgram <- lookupProgram state resId
  case maybeProgram of
    Just prog -> do
      let answer = map (\(l, w, _) -> PatternInfo l w) $ patternList prog
      respond $ jsonResponse status200 answer
    Nothing   -> notFound request respond
  
handleNotAllowed :: [ByteString] -> Application
handleNotAllowed allow _ respond = 
    respond $ responseLBS status405 [("Allow", "," `BS.intercalate` allow)] ""

notFound :: Application
notFound _ respond = respond $ textResponse status404 "Resource Not Found"

jsonResponse :: ToJSON a => Status -> a -> Response
jsonResponse status = 
    responseLBS status [("Content-Type", "application/json")] . encode

textResponse :: Status -> LBS.ByteString -> Response
textResponse status = responseLBS status [("Content-Type", "text/plain")]

-- | Strictly decode the body to a JSON data structure. May throw an
-- exception if the decoding is failing.
decodeBody :: FromJSON a => Request -> IO a
decodeBody req = fromJust . decode' <$> lazyRequestBody req

-- | Output a line in the log. The line is prepended by the current time (UTC).
logg :: State -> String -> IO ()
logg State {..} str = do
  now <- getCurrentTime
  out $ show now `mappend` " : " `mappend` str
    where
      out :: String -> IO ()
      out = pushLogStrLn logger . toLogStr
  

