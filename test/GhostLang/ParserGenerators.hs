{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.ParserGenerators where

import Control.Monad (forM_)
import Control.Monad.Writer (execWriter, tell)
import GhostLang.Types ( ModuleSegment
                       , GhostModule (..)
                       , ModuleDecl (..)
                       , ImportDecl (..)
                       )
import Test.QuickCheck
import Text.Printf (printf)
import qualified Data.Text as T

-- | Typeclass to "stringify", i.e. convert a data structure to real
-- syntax.
class Stringify a where
    stringify :: a -> String

-- | Arbitrary instance for GhostModule.
instance Arbitrary GhostModule where
    arbitrary = GhostModule <$> arbitrary <*> listOf arbitrary

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

instance Stringify GhostModule where
    stringify (GhostModule modDecl impDecls) =
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




