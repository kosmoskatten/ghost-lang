{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.ParserGenerators where

import Control.Monad (forM_)
import Control.Monad.Writer (execWriter, tell)
import GhostLang.CommonGenerators ()
import GhostLang.Intrinsic (IntrinsicSet)
import GhostLang.Types ( Label
                       , ModuleSegment
                       , GhostModule (..)
                       , ModuleDecl (..)
                       , ImportDecl (..)
                       , Value (..)
                       )
import Test.QuickCheck
import Text.Printf (printf)
import qualified Data.Text as T

-- | Typeclass to "stringify", i.e. convert a data structure to real
-- syntax.
class Stringify a where
    stringify :: a -> String

-- | Arbitrary instance for GhostModule.
instance Arbitrary (GhostModule a) where
    arbitrary = GhostModule <$> arbitrary <*> listOf arbitrary
                            <*> pure []   <*> pure []

-- | Arbitrary instance for ModuleDecl.
instance Arbitrary ModuleDecl where
    arbitrary = ModuleDecl <$> listOf1 arbitrary

-- | Arbitrary instance for ImportDecl.
instance Arbitrary ImportDecl where
    arbitrary = ImportDecl <$> listOf1 arbitrary

-- | Arbitrary instance for ModuleSegment. Only generates valid
-- segments.
instance Arbitrary ModuleSegment where
    arbitrary = T.pack <$> moduleSegment
        where
          moduleSegment = 
              (:) <$> elements ['A'..'Z']
                  <*> listOf (elements $ ['a'..'z'] ++
                                         ['0'..'9'] ++ "-_")

instance Stringify (GhostModule a) where
    stringify (GhostModule modDecl impDecls _ _) =
        execWriter $ do
          tell $ printf "%s\n\n" (stringify modDecl)
          forM_ impDecls $ \impDecl ->
              tell $ printf "%s\n" (stringify impDecl)

instance Stringify ModuleDecl where
    stringify (ModuleDecl segs) = printf "module %s" (str segs)
        where str = T.unpack . T.intercalate "."

instance Stringify ImportDecl where
    stringify (ImportDecl segs) = printf "import %s" (str segs)
        where str = T.unpack . T.intercalate "."

instance Stringify Value where
    stringify (Literal v)      = printf "literal(%ld)" v
    stringify (Stored v)       = printf "%s" (T.unpack v)
    stringify (Gaussian v1 v2) = printf "gaussian(%ld, %ld)" v1 v2
    stringify (Uniform v1 v2)  = printf "uniform(%ld, %ld)" v1 v2


