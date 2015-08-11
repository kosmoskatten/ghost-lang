module GhostLang.Interpreter.WebClient
    ( httpGet
    , eagerConsumer
    ) where

import Data.ByteString (ByteString)
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Data.ByteString as BS

-- | Fetch a web resource using the provided consumer.
httpGet :: Manager -> String -> (Response BodyReader -> IO Status) -> IO Status
httpGet mgr url consumer = do
  req <- parseUrl url
  withResponse req mgr consumer

-- | The eager consumer. Consume as fast as possible from the network.
eagerConsumer :: Response BodyReader -> IO Status
eagerConsumer resp = do
  let status = responseStatus resp
      body   = responseBody resp

  consumeIt body =<< brRead body
  return status
    where
      consumeIt :: BodyReader -> ByteString -> IO ()
      consumeIt body bs
          | BS.null bs   = return ()
          | otherwise = do
                       bs' <- brRead body
                       putStrLn "Got chunk"
                       consumeIt body bs'
