module GhostLang.Node.IdGen
    ( genId
    ) where

import Codec.Binary.Base64.String (encode)
import Control.Monad (replicateM)
import Data.Text (Text)
import System.Random (randomRIO)
import qualified Data.Text as T

-- | Generate a random Base64 encoded string.
genId :: IO Text
genId = T.pack . encode <$> randomString

randomString :: IO String
randomString = replicateM idLength $ randomRIO (' ', '~')

idLength :: Int
idLength = 21
