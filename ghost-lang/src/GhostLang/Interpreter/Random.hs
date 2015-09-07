module GhostLang.Interpreter.Random
    ( GenIO
    , atLeastZero
    , gaussianValue
    , uniformValue
    , createSystemRandom
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Int (Int64)
import System.Random.MWC (GenIO, createSystemRandom, uniformR)
import System.Random.MWC.Distributions (normal)

-- | Generate a gaussian value with the given mean and stddev values.
gaussianValue :: MonadIO m => Int64 -> Int64 -> GenIO -> m Int64
gaussianValue mean stddev gen = do
    let mean'   = fromIntegral mean
        stddev' = fromIntegral stddev
    round <$> (liftIO $ normal mean' stddev' gen)

-- | Generate a uniform value within the range.
uniformValue :: MonadIO m => (Int64, Int64) -> GenIO -> m Int64
uniformValue range gen = liftIO $ uniformR range gen

-- | Adjust a value to always be zero or above.
atLeastZero :: Int64 -> Int64
atLeastZero n
    | n < 0     = 0
    | otherwise = n

    
