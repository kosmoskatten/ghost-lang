module GhostLang.Node.IdGen
    ( genId
    ) where

import Codec.Binary.Base64.String (encode)
import Control.Monad (replicateM)
import System.Random (randomRIO)

-- | Generate a random Base64 encoded string.
genId :: IO String
genId = encode <$> randomString

randomString :: IO String
randomString = replicateM idLength $ randomRIO (' ', '~')

idLength :: Int
idLength = 21
