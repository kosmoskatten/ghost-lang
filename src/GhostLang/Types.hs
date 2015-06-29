{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE GADTs                #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.Types
    ( Label
    , Weight
    , Program (..)
    , Pattern (..)
    , Procedure (..)
    , Operation (..)
    ) where

import Data.Serialize (Serialize (..), getByteString, putByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import GHC.Generics (Generic)

import qualified Data.Text as T

type Label = Text
type Weight = Int

data Program a where
    Program :: !Text -> ![Pattern a] -> Program a
    deriving (Eq, Generic, Show)

data Pattern a where
    Pattern :: !Label -> !Weight -> ![Operation a] -> Pattern a
    deriving (Eq, Generic, Show)

data Procedure a where
    Procedure :: !Label -> ![Operation a] -> Procedure a
    deriving (Eq, Generic, Show)

data Operation a where
    Invoke :: !a -> Operation a
    -- ^ Invoke is the operation of invoking a simple element of the
    -- ghost language. E.g. invoking a http get.

    Call :: !(Procedure a) -> Operation a
    -- ^ Call is the operation of calling a procedure. The procedure
    -- is given its arguments in a local context of the Interpreter
    -- monad.

    Unresolved :: !Label -> Operation a
    deriving (Eq, Generic, Show)

instance Serialize a => Serialize (Program a)
instance Serialize a => Serialize (Pattern a)
instance Serialize a => Serialize (Procedure a)
instance Serialize a => Serialize (Operation a)

-- | Serialize instance for Text as it's not included in the Cereal
-- library.
instance Serialize Text where
    put t = do
      put $ T.length t
      putByteString $ encodeUtf8 t

    get = do
      len <- get
      decodeUtf8 <$> getByteString len
