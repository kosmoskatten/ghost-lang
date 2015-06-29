module GhostLang.SerializationTests
    ( encodeDecodeIsEqual
    ) where

import Data.Serialize (decode, encode)
import GhostLang.Generators (TestInstrSet)
import GhostLang.Types (Program)
import Test.QuickCheck

-- | Test that encoding followed by a decode result in the original
-- value.
encodeDecodeIsEqual :: Program TestInstrSet -> Bool
encodeDecodeIsEqual p =
    case decode (encode p) of
      Right p' -> p == p'
      _        -> False
