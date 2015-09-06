{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
module GhostLang.Conduit
    ( DataChunk
    , byteCounter
    , dataStream
    , shape
    , genDataChunk
    , module Data.Conduit
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import Control.Monad.Trans.Resource (MonadResource, liftResourceT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Time ( NominalDiffTime
                 , UTCTime
                 , diffUTCTime
                 , getCurrentTime
                 )
import System.Random (randomRIO)
import qualified Data.ByteString.Char8 as BS

data DataChunk = DataChunk { chunk :: !ByteString
                           , size  :: !Int }
    deriving Show

-- | A virtual buffer is a tool for simulation of application level
-- buffering. The buffer will approximate a pace, and that pace will
-- cause back pressure on the network if necessary.
data VirtualBuffer =
    VirtualBuffer { totalRec    :: !Int
                  , bufferPace  :: !Int
                  , bufferStart :: !UTCTime
                  }
    deriving Show

-- | Count and discard the incoming data stream. 
byteCounter :: Monad m => Sink ByteString m Int
byteCounter = go 0 =<< await
    where
      go :: Monad m => Int -> Maybe ByteString -> Sink ByteString m Int
      go !acc (Just bs) = go (acc + BS.length bs) =<< await
      go !acc Nothing   = return acc

-- | Generate a stream of 'n' bytes data from the provided data chunk.
dataStream :: DataChunk -> Int -> Source IO ByteString
dataStream DataChunk {..} n = go n
    where
      go :: Int -> Source IO ByteString
      go !left
          | left == 0    = return ()
          | left >= size = do
              yield chunk
              go (left - size)
          | otherwise    = do
              yield (BS.take left chunk)
              return ()

-- | A shaping conduit that emulates a buffer, with a buffer reading
-- pace of 'pace' bytes per second.
shape :: MonadIO m => Int -> Conduit ByteString m ByteString
shape pace = consumeStream =<< (initVirtualBuffer pace)
    where
      consumeStream vb = do
        full <- isFull vb
        if full then do waitAWhile
                        consumeStream vb
        else do maybeChunk <- await
                case maybeChunk of
                  Just chunk -> do
                    yield chunk
                    consumeStream $ addData (BS.length chunk) vb
                  Nothing    -> return ()

-- | Generate a data chunk of the requested size n.
genDataChunk :: Int -> IO DataChunk
genDataChunk n = do
  chunk' <- BS.pack <$> (replicateM n $ randomRIO (' ', '~'))
  return $ DataChunk { chunk = chunk', size = n }

-- | Initialize the virtual buffer the pace, expressed as the number
-- of bytes that are allowed to pass per second (statistically).
initVirtualBuffer :: MonadIO m => Int -> m VirtualBuffer
initVirtualBuffer pace = do
  start <- liftIO getCurrentTime
  return $! VirtualBuffer { totalRec    = 0
                          , bufferPace  = pace
                          , bufferStart = start
                          }

addData :: Int -> VirtualBuffer -> VirtualBuffer
addData amount buffer = buffer { totalRec = amount + totalRec buffer }

isFull :: MonadIO m => VirtualBuffer -> m Bool
isFull buffer = do
  now <- liftIO getCurrentTime
  let duration = now `diffUTCTime` bufferStart buffer
      tp       = totalRec buffer `throughput` duration
  return $! tp > bufferPace buffer

throughput :: Int -> NominalDiffTime -> Int
throughput amount duration =
    let duration' = realToFrac duration :: Double
        rate      = fromIntegral amount / duration'
    in round rate

waitAWhile :: MonadIO m => m ()
waitAWhile = liftIO $ threadDelay 100000

