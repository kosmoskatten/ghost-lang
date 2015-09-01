{-# LANGUAGE RecordWildCards #-}
module GhostLang.ConduitProps
    ( sourceAndSinkShallMatch
    ) where

import GhostLang.Conduit ( ($$)
                         , genDataChunk
                         , byteCounter
                         , dataStream
                         )

import Test.QuickCheck
import Test.QuickCheck.Monadic

data TestSpec = TestSpec { chunkSize  :: !Int
                         , streamSize :: !Int
                         }
    deriving Show

instance Arbitrary TestSpec where
    -- Let chunk size vary between 1 and 32 K. The stream vary from
    -- zero and up.
    arbitrary = TestSpec <$> choose (1, 32768) <*> choose (0, 1000000)

-- | Given arbitrary size of chunks and streams, the amount generated
-- from the source shall match the amount reported from the sink.
sourceAndSinkShallMatch :: TestSpec -> Property
sourceAndSinkShallMatch TestSpec {..} = 
    monadicIO $ do
      chunk  <- run $ genDataChunk chunkSize
      amount <- run $ dataStream chunk streamSize $$ byteCounter
      assert $ streamSize == amount
