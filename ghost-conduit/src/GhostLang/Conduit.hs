{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
module GhostLang.Conduit
    ( DataChunk
    , byteCounter
    , dataStream
    , genDataChunk
    , module Data.Conduit
    ) where

import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.Conduit
import System.Random (randomRIO)
import qualified Data.ByteString.Char8 as BS

data DataChunk = DataChunk { chunk :: !ByteString
                           , size  :: !Int }
    deriving Show

-- | Count and discard the incoming data stream.
byteCounter :: Sink ByteString IO Int
byteCounter = go 0 =<< await
    where
      go :: Int -> Maybe ByteString -> Sink ByteString IO Int
      go !acc (Just bs) = go (acc + BS.length bs) =<< await
      go !acc Nothing   = return acc

-- | Generate a stream of data from the provided data chunk.
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

-- | Generate a data chunk of the requested size n.
genDataChunk :: Int -> IO DataChunk
genDataChunk n = do
  chunk' <- BS.pack <$> (replicateM n $ randomRIO (' ', '~'))
  return $ DataChunk { chunk = chunk', size = n }

