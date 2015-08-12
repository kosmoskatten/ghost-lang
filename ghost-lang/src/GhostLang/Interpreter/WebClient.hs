module GhostLang.Interpreter.WebClient
    ( httpGet
    , eagerConsumer
    ) where

import Data.ByteString (ByteString)
import Data.Time ( NominalDiffTime
                 , UTCTime
                 , diffUTCTime
                 , getCurrentTime
                 )
import GHC.Int (Int64)
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Data.ByteString as BS

data VirtualBuffer =
    VirtualBuffer { totalRec    :: !Int
                  , bufferPace  :: !Int64
                  , bufferStart :: !UTCTime
                  }
    deriving Show

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

{-pacedConsumer :: VirtualBuffer-> Response BodyReader -> IO Status
pacedConsumer buffer resp = do
  let status = responseStatus resp
      body   = responseBody resp-}

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
  putStrLn $ show tp
  return $ tp > bufferPace buffer

throughput :: Int -> NominalDiffTime-> Int64
throughput amount duration =
    let duration' = realToFrac duration :: Double
        rate      = fromIntegral amount / duration'
    in round rate
  
