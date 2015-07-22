{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE GADTs                #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.Types
    ( Label
    , ModuleSegment
    , Weight
    , GhostModule (..)
    , ModuleDecl (..)
    , ImportDecl (..)
    , TimeUnit (..)
    , Value (..)
    , Program (..)
    , Pattern (..)
    , Procedure (..)
    , Operation (..)
    ) where

import Data.Serialize (Serialize (..), getByteString, putByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Text.Parsec.Pos ( SourcePos
                       , sourceName
                       , sourceLine
                       , sourceColumn
                       , newPos
                       )

import qualified Data.Text as T

type Label         = Text
type ModuleSegment = Text
type Weight        = Int64

-- | Description of a ghost-lang module. This is the representation
-- produced of the parser when reading a gl-file. One or several ghost
-- modules are linked to a program.
data GhostModule a where
    GhostModule :: ModuleDecl -> [ImportDecl] 
                -> [Pattern a] -> [Procedure a] -> GhostModule a
    deriving (Eq, Generic, Show)

-- | A module declaration, specifying the full path name of the
-- module.
newtype ModuleDecl = ModuleDecl [ModuleSegment]
    deriving (Eq, Generic, Show)

-- | An import declaration, specifying the full path name of the
-- imported module.
newtype ImportDecl = ImportDecl [ModuleSegment]
    deriving (Eq, Generic, Show)

-- | A unit for describing size of time.
data TimeUnit = USec Value | MSec Value | Sec Value
    deriving (Eq, Generic, Show)

-- | Representation of numeric values in ghost-lang.
data Value = Literal !Int64
           -- ^ A constant, literal, value.
           | Uniform !Int64 !Int64
           -- ^ A randomly - uniform - distributed value.
           | Gaussian !Int64 !Int64
           -- ^ A randomly - normal - distributed value.
           | Stored !Label
            -- ^ An indirect value, looked up from store by
            -- name. After lookup is become one of the above.
    deriving (Eq, Generic, Show)

-- | A representation of a ghost-lang program. A program has a name
-- and a set of patterns.
data Program a where
    Program :: !Label -> ![Pattern a] -> Program a
    deriving (Eq, Generic, Show)

-- | Pattern is a top level procedure carrying a statistical weight
-- and a set of operations.
data Pattern a where
    Pattern :: !Label -> !Weight -> ![Operation a] -> Pattern a
    deriving (Eq, Generic, Show)

-- | Procedure is a ghost-lang construct for making reusable building
-- blocks. A procedure is carrying a set of parameter names and a set
-- of operations.
data Procedure a where
    Procedure :: !Label -> ![Label] -> ![Operation a] -> Procedure a
    deriving (Eq, Generic, Show)

-- | Operation is the lowest level entity that can be controlled in a
-- ghost-lang program.
data Operation a where
    Invoke :: !a -> Operation a
    -- ^ Invoke is the operation of invoking a simple element of the
    -- ghost language. E.g. invoking a http get.

    Loop :: !Value -> ![Operation a] -> Operation a
    -- ^ Loop the set of operations the number of times specified by
    -- the value.

    Concurrently :: ![Operation a] -> Operation a
    -- ^ Concurrently execute the operations. The Concurrently command
    -- is ready when the last concurrent operation is done.

    Call :: !(Procedure a) -> ![Value] -> Operation a
    -- ^ Call is the operation of calling a procedure. The procedure
    -- is given its arguments in a local context of the Interpreter
    -- monad.

    Unresolved :: !SourcePos -> !Label -> ![Value] -> Operation a
    -- ^ Unresolved procedure.
    deriving (Generic, Show)

instance Serialize Value
instance Serialize a => Serialize (Program a)
instance Serialize a => Serialize (Pattern a)
instance Serialize a => Serialize (Procedure a)
instance Serialize a => Serialize (Operation a)

-- | Tailor made an Eq instance for Operation which not include
-- SourcePos. Needed for testing purposes.
instance Eq a => Eq (Operation a) where
    (Invoke x)         == (Invoke x')          = x == x'
    (Loop x y)         == (Loop x' y')         = x == x' && y == y'
    (Concurrently x)   == (Concurrently x')    = x == x'
    (Call x y)         == (Call x' y')         = x == x' && y == y'
    (Unresolved _ x y) == (Unresolved _ x' y') = x == x' && y == y'
    _                  == _                    = False

-- | Serialize instance for Text as it's not included in the Cereal
-- library.
instance Serialize Text where
    put t = do
      put $ T.length t
      putByteString $ encodeUtf8 t

    get = do
      len <- get
      decodeUtf8 <$> getByteString len

-- | Ditto SourcePos.
instance Serialize SourcePos where
    put s = do
      put $ sourceName s
      put $ sourceLine s
      put $ sourceColumn s

    get = newPos <$> get <*> get <*> get
