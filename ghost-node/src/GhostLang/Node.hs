{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | The GhostLang.Node module is implementing most of the HTTP
-- related stuff. E.g. routing, handling of responses and
-- encoding/decoding.
module GhostLang.Node
    ( runNode
    ) where

import Data.ByteString (ByteString)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Time (getCurrentTime)
import Data.Text (Text)
import GhostLang.API ( ProgramPath (..)
                     , PatternInfo (..)
                     , Resource (..)
                     , NamedPattern (..)
                     , ExecParams (..)
                     , FromJSON
                     , ToJSON
                     , encode
                     , decode' )
import GhostLang.Node.Flow ( getHttpConfig
                           , setHttpConfig
                           , listPrograms
                           , listPatternsFromProgram
                           , loadProgram
                           )
import GhostLang.Node.IdGen (genId)
import GhostLang.Node.State ( State (..)
                            , ResourceKey
                            , ProgramRepr (..)
                            , initState
                            , lookupProgram
                            )
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
    response <-
        case pathInfo request of
          -- Request to read or set the http configuration.
          ["configuration", "http"]
            | requestMethod request == "GET"  -> 
                handleGetHttpConfig state
            | requestMethod request == "PUT" ->
                handleSetHttpConfig state request
            | otherwise                       -> 
                handleNotAllowed ["GET", "PUT"]
                                       
          -- Route a request for compiling and loading a
          -- ghost-program. Only POST request are accepted.
          ["program", "load"]
            | requestMethod request == "POST" -> 
                handleProgramLoad state request
            | otherwise                       -> 
                handleNotAllowed ["POST"]
                                       
          -- Route a request for listing all the resource ids for the
          -- programs loaded.
          ["program", "list"]
            | requestMethod request == "GET" ->
                handleProgramList state
            | otherwise                      ->
                handleNotAllowed ["GET"]
                                                                            
          -- Route a request for listing all the patterns contained in a
          -- selected compiled ghost-program. Only GET requests are
          -- accepted.
          ["program", key, "list"]
            | requestMethod request == "GET" ->
                handleSelectedProgramList state key
            | otherwise                      ->
                handleNotAllowed ["GET"]

           -- Route a request for running a named pattern from the
           -- selected ghost-program. Only POST requests are accepted.
          ["program", key, "named-pattern"]
            | requestMethod request == "POST" ->
                handleNamedPatternRun state request key
            | otherwise                       ->
                handleNotAllowed ["POST"]
                                                             
          -- No matching handler is found.
          _ -> return notFound

    
    -- Logg the request and the response.
    let status = responseStatus response
    logg state $ printf "%s %s %d/%s" (BS.unpack $ requestMethod request) 
                                      (BS.unpack $ rawPathInfo request)
                                      (statusCode status)
                                      (BS.unpack $ statusMessage status)

    -- Respond back to Wai.
    respond response
                                            
-- | Get the http configuration. Return the configuration as JSON
-- with response code 200.
handleGetHttpConfig :: State -> IO Response
handleGetHttpConfig state = jsonResponse status200 <$> getHttpConfig state

-- | Set the http configuration carried in a Service record. Respond
-- with an empty response code 200.
handleSetHttpConfig :: State -> Request -> IO Response
handleSetHttpConfig state request = do
  setHttpConfig state =<< decodeBody request
  return $ textResponse status200 ""

-- | Handle the request of compiling and loading a ghost-program. If
-- the compilation is successful, the compiled program is stored and a
-- response code 201 carrying a JSON with the resource id is
-- returned. Otherwise an error with code 409 is returned.
handleProgramLoad :: State -> Request -> IO Response
handleProgramLoad state request = do
  result <- loadProgram state =<< decodeBody request
  case result of
    Right answer -> return $ jsonResponse status201 answer
    Left err     -> return $ textResponse status409 (LBS.pack err)

-- | List the resource ids for all loaded programs. Always 200 as
-- response.
handleProgramList :: State -> IO Response
handleProgramList state = jsonResponse status200 <$> listPrograms state

-- | List the patterns for the selected program. The response code is
-- 200 if the program is found, otherwise 404.
handleSelectedProgramList :: State -> ResourceKey -> IO Response
handleSelectedProgramList state key =
    maybe notFound (jsonResponse status200) 
          <$> listPatternsFromProgram state key

-- | Run a named pattern from the selected program. If the pattern is
-- found a response code 201 is returned. If the pattern is not found
-- 409 is returned, if the program not is found 404 is returned.
handleNamedPatternRun :: State -> Request -> ResourceKey -> IO Response
handleNamedPatternRun state request resId = do
  maybeProgram <- lookupProgram state resId
  case maybeProgram of
    Just prog -> do
      msg <- decodeBody request
      case find (\(l, _, _) -> l == execPattern msg) $ patternList prog of
        Just (_, _, p) -> do
            id' <- genId            
            let patternId = "/pattern/" `mappend` id'
                answer    = Resource { resourceId = patternId }
            return $ jsonResponse status201 answer
        Nothing        -> return $ textResponse status409 "Pattern not found"
    Nothing  -> return notFound

handleNotAllowed :: [ByteString] -> IO Response
handleNotAllowed allow = 
    return $ responseLBS status405 [("Allow", "," `BS.intercalate` allow)] ""

notFound :: Response
notFound = textResponse status404 "Resource Not Found"

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
  

