{-# LANGUAGE OverloadedStrings #-}
module GhostLang.Server
    ( runServer
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteString)
import GhostLang.Conduit ( Conduit
                         , DataChunk
                         , Flush (..)
                         , ($$)
                         , ($=)
                         , awaitForever
                         , byteCounter
                         , dataStream
                         , genDataChunk
                         , shape
                         , yield
                         )
import GhostLang.GLog ( GLog
                      , newStdoutGLog
                      , logHttpReqResp
                      , logString
                      )
import Network.Wai.Conduit
import Network.HTTP.Types
import Network.Wai.Handler.Warp ( HostPreference
                                , defaultSettings
                                , setHost
                                , setPort
                                , runSettings
                                )
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as BS

data State = State { logger :: !GLog
                   , chunk  :: !DataChunk
                   }

runServer :: HostPreference -> Int -> IO ()
runServer hostPreference listenPort = do
    state <- State <$> newStdoutGLog <*> genDataChunk 32768
    logString (logger state) $ 
        printf "Ghost Server listening on %s:%d"
            (show hostPreference) listenPort
    let settings = setHost hostPreference $ 
                   setPort listenPort $
                   defaultSettings

    runSettings settings $ application state

-- | Application entry per request.
application :: State -> Application
application state request respond = do
  response <- route state request
  let status = responseStatus response
                  
  logHttpReqResp (logger state) (requestMethod request)
                 (rawPathInfo request `BS.append` rawQueryString request) 
                 (statusCode status)
                 (statusMessage status)

  respond response

-- | Route the request to the correct handler.
route :: State -> Request -> IO Response
route state request = do
  case rawPathInfo request of
    "/download"
        | requestMethod request == "GET" -> handleDownload state request
        | otherwise                      -> handleNotAllowed ["GET"]

    "/upload"
        | requestMethod request == "PUT" -> handleUpload state request
        | otherwise                      -> handleNotAllowed ["PUT"]
    _                                    -> return notFound

-- | Handle a download request. The size of the request is attached as
-- a query parameter to the request.
handleDownload :: State -> Request -> IO Response
handleDownload state request =
    case requestedSize request of
      Just amount -> 
          return $ responseSource status200
                     [("Content-Type", "text/plain")]
                     (dataStream (chunk state) amount $= streamConverter)
      Nothing     -> 
          return $ responseLBS status400
                     [("Content-Type", "text/plain")]
                     "Missing or malformed size parameter"

handleUpload :: State -> Request -> IO Response
handleUpload state request = do
  bytes <- 
      case requestedPace request of
        Just pace -> sourceRequestBody request $= shape pace $$ byteCounter
        Nothing   -> sourceRequestBody request $$ byteCounter
  logString (logger state) $ printf "DEBUG: received %d bytes" bytes
  return $ responseLBS status200 [] ""

-- | Handle non allowed method by returning status 405.
handleNotAllowed :: [ByteString] -> IO Response
handleNotAllowed allow = 
    return $ responseLBS status405 [("Allow", "," `BS.intercalate` allow)] ""

-- | Make a 404 response.
notFound :: Response
notFound = responseLBS status404 
                       [("Content-Type", "text/plain")] 
                       "Resource not found"

-- | Get the requested size from the query parameter named "size".
requestedSize :: Request -> Maybe Int
requestedSize = requestedIntValue "size"

-- | Get the requested pace from the query parameter named "pace".
requestedPace :: Request -> Maybe Int
requestedPace = requestedIntValue "pace"

requestedIntValue :: ByteString -> Request -> Maybe Int
requestedIntValue str req = do
  val  <- lookup str $ queryString req
  val' <- BS.unpack <$> val -- Also evaluating second level of Maybe
  maybeInt val'

maybeInt :: String -> Maybe Int
maybeInt str =
    case reads str of
      [(value, "")] -> Just value
      _             -> Nothing

-- | Conduit to convert from a stream of bytestrings to a stream of
-- builders.
streamConverter :: Conduit ByteString IO (Flush Builder)
streamConverter = awaitForever (yield . Chunk . byteString)

