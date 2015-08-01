module GhostLang.Serialize
    ( fromFile
    , toFile
    ) where

import Conduit (($$), runResourceT, sinkFile, sourceFile)
import Data.Conduit.Cereal (sinkGet, sourcePut)
import Data.Serialize (Serialize (..))
import GhostLang.Types (Program)

-- | Read a program from a serialized file.
fromFile :: Serialize a => FilePath -> IO (Program a)
fromFile file = runResourceT $ sourceFile file $$ sinkGet get

-- | Serialize the program to file.
toFile :: Serialize a => Program a -> FilePath -> IO ()
toFile program file = runResourceT $ sourcePut (put program) $$ sinkFile file
