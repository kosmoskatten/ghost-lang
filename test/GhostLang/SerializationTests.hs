{-# LANGUAGE OverloadedStrings #-}
module GhostLang.SerializationTests
    ( encodeDecodeIsEqual
    , writeReadFile
    ) where

import Control.Exception (bracket)
import Data.Serialize (decode, encode)
import GhostLang.Generators (TestInstrSet (..))
import GhostLang.Serialize (fromFile, toFile)
import GhostLang.Types (Program (..), Pattern (..), Operation (..))
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath
import System.Posix.Process (getProcessID)
import Test.HUnit

-- | Test that encoding followed by a decode result in the original
-- value.
encodeDecodeIsEqual :: Program TestInstrSet -> Bool
encodeDecodeIsEqual p =
    case decode (encode p) of
      Right p' -> p == p'
      _        -> False

-- | Write a simple Program to file and read back again. Shall be
-- equal to the original value.
writeReadFile :: Assertion
writeReadFile = 
    bracket getTempFilePath
            removeFile
            (\file -> do
               let program = Program "program" 
                             [ Pattern "pattern" 1
                               [ Invoke Instr1
                               ]
                             ]
               program `toFile` file
               program' <- fromFile file
               program @=? program'
            )
            
getTempFilePath :: IO FilePath
getTempFilePath = do
  tmp <- getTemporaryDirectory
  pid <- getProcessID
  return $ tmp </> show pid <.> "bin"
