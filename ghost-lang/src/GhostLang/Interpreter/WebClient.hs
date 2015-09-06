{-# LANGUAGE OverloadedStrings #-}
-- | Low level HTTP functions.
module GhostLang.Interpreter.WebClient
    ( httpGet
    , httpPut
    ) where

import Control.Monad (void)
import Control.Monad.Trans.Resource (runResourceT)
import GHC.Int (Int64)
import GhostLang.Conduit ( DataChunk
                         , ($=+)
                         , ($$+-)
                         , byteCounter
                         , dataStream
                         , shape 
                         )
import GhostLang.RuntimeState (HttpStatus (..))
import Network.HTTP.Conduit
import Network.HTTP.Types

type RetVal = (HttpStatus, Int64)

-- | Fetch a web resource.
httpGet :: Manager -> String -> Maybe Int -> IO RetVal
httpGet mgr url maybePace = do
  req <- parseUrl url
  runResourceT $ do
    resp <- http req mgr
    bytes <- 
        case maybePace of
          Just pace -> responseBody resp $=+ shape pace $$+- byteCounter
          Nothing   -> responseBody resp $$+- byteCounter
    return (fromStatus $ responseStatus resp, fromIntegral bytes)

-- | Upload data to a web resource.
httpPut :: Manager -> String -> DataChunk -> Int -> IO RetVal
httpPut mgr url dc bytes = do
  req <- parseUrl url
  let body = requestBodySourceChunkedIO $ dataStream dc bytes
  let req' = req { method = "PUT", requestBody = body }
  runResourceT $ do
    resp <- http req' mgr
    void $ responseBody resp $$+- byteCounter -- Expect no payload data.
    return (fromStatus $ responseStatus resp, fromIntegral bytes)
  
fromStatus :: Status -> HttpStatus
fromStatus status
    | status == status200 = Success
    | otherwise           = Failure
