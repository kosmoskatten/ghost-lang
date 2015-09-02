{-# LANGUAGE OverloadedStrings #-}
module GhostLang.Server
    ( runServer
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteString)
import GhostLang.Conduit ( Conduit
                         , DataChunk
                         , Flush (..)
                         , ($=)
                         , await
                         , dataStream
                         , genDataChunk
                         , yield
                         )
import GhostLang.GLog ( GLog
                      , newStdoutGLog
                      , logHttpReqResp
                      , logString
                      )
import Network.Wai.Conduit
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as BS

data State = State { logger :: !GLog
                   , chunk  :: !DataChunk
                   }

runServer :: Int -> IO ()
runServer port = do
  state <- State <$> newStdoutGLog <*> genDataChunk 32768
  logString (logger state) $ printf "Ghost Server listening on port %d" port
  run port $ application state

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
    _                                    -> return notFound

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

handleNotAllowed :: [ByteString] -> IO Response
handleNotAllowed allow = 
    return $ responseLBS status405 [("Allow", "," `BS.intercalate` allow)] ""

notFound :: Response
notFound = responseLBS status404 
                       [("Content-Type", "text/plain")] 
                       "Resource not found"

requestedSize :: Request -> Maybe Int
requestedSize req = do
  val  <- lookup "size" $ queryString req
  val' <- BS.unpack <$> val -- Also evaluating second level of Maybe
  maybeInt val'

maybeInt :: String -> Maybe Int
maybeInt str =
    case reads str of
      [(value, "")] -> Just value
      _             -> Nothing

streamConverter :: Conduit ByteString IO (Flush Builder)
streamConverter = go =<< await
    where
      go :: Maybe ByteString -> Conduit ByteString IO (Flush Builder)
      go (Just bs) = do
        yield $ Chunk (byteString bs)
        go =<< await
      go Nothing   = return ()

