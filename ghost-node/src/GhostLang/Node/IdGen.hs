{-# LANGUAGE OverloadedStrings #-}
module GhostLang.Node.IdGen
    ( genId
    ) where

import Control.Monad (replicateM)
import Data.Text (Text)
import System.Random (randomRIO)
import qualified Data.Text as T

-- | Generate a random text string.
genId :: IO Text
genId = T.pack <$> randomString

randomString :: IO String
randomString = do
  let len = T.length charSet - 1
  replicateM idLength $ randomChar len
    where randomChar len = T.index charSet <$> randomRIO (0, len)

charSet :: Text
charSet = "ABCDEFGHIJKLMNOPQRTSUVWXYZabcdefghijklmnopqrtsuvwxyz012345789"

idLength :: Int
idLength = 27
