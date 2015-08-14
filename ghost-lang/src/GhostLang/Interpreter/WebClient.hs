module GhostLang.Interpreter.WebClient
    ( httpGet
    , eagerConsumer
    , pacedConsumer
    , initVirtualBuffer
    ) where

import Control.Concurrent (threadDelay)
import Data.ByteString (ByteString)
import Data.Time ( NominalDiffTime
                 , UTCTime
                 , diffUTCTime
                 , getCurrentTime
                 )
import GHC.Int (Int64)
import GhostLang.RuntimeState (HttpStatus (..))
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Data.ByteString as BS

import Text.Printf (printf)

type RetVal = (HttpStatus, Int64)

-- | A virtual buffer is a tool for simulation of application level
-- buffering. The buffer will approximate a pace, and that pace will
-- cause back pressure on the network if necessary.
data VirtualBuffer =
    VirtualBuffer { totalRec    :: !Int
                  , bufferPace  :: !Int64
                  , bufferStart :: !UTCTime
                  }
    deriving Show

-- | Fetch a web resource using the provided consumer.
httpGet :: Manager -> String -> (Response BodyReader -> IO RetVal) -> IO RetVal
httpGet mgr url consumer = do
  req <- parseUrl url
  withResponse req mgr consumer

-- | The eager consumer. Consume as fast as possible from the network.
eagerConsumer :: Response BodyReader -> IO RetVal
eagerConsumer resp = do
  let status = responseStatus resp
      body   = responseBody resp

  bs  <- brRead body
  len <- consumeIt (BS.length bs) body bs
  return (fromStatus status, fromIntegral len)
    where
      consumeIt :: Int -> BodyReader -> ByteString -> IO Int
      consumeIt acc body bs
          | BS.null bs = return acc
          | otherwise  = do
              bs' <- brRead body
              consumeIt (acc + BS.length bs') body bs'

-- | The paced consumer. Is using the buffer for paceing.
pacedConsumer :: VirtualBuffer -> Response BodyReader -> IO RetVal
pacedConsumer buffer resp = do
  let status = responseStatus resp
      body   = responseBody resp

  len <- consumeIt buffer body
  return (fromStatus status, fromIntegral len)
      where
        consumeIt :: VirtualBuffer -> BodyReader -> IO Int
        consumeIt buf body = do
            full <- isFull buf
            if full then 
                do threadDelay 100000
                   consumeIt buf body
            else do
              bs <- brRead body
              if not (BS.null bs) then
                  do let buf' = addData (BS.length bs) buf
                     consumeIt buf' body
              else return $ totalRec buf

-- | Initialize the virtual buffer the pace, expressed as the number
-- of bytes that are allowed to pass per second (statistically).
initVirtualBuffer :: Int64 -> IO VirtualBuffer
initVirtualBuffer pace = do
  start <- getCurrentTime
  return $! VirtualBuffer { totalRec    = 0
                          , bufferPace  = pace
                          , bufferStart = start
                          }

addData :: Int -> VirtualBuffer -> VirtualBuffer
addData amount buffer = buffer { totalRec = amount + totalRec buffer }

isFull :: VirtualBuffer -> IO Bool
isFull buffer = do
  now <- getCurrentTime
  let duration = now `diffUTCTime` bufferStart buffer
      tp       = totalRec buffer `throughput` duration
  return $! tp > bufferPace buffer

throughput :: Int -> NominalDiffTime-> Int64
throughput amount duration =
    let duration' = realToFrac duration :: Double
        rate      = fromIntegral amount / duration'
    in round rate
  
fromStatus :: Status -> HttpStatus
fromStatus status
    | status == status200 = Success
    | otherwise           = Failure
