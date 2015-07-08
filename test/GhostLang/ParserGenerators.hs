{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.ParserGenerators where

import GhostLang.Types (ModuleSegment, ModuleDecl (..))
import Test.QuickCheck
import Text.Printf (printf)
import qualified Data.Text as T

class Stringify a where
    stringify :: a -> String

instance Arbitrary ModuleDecl where
    arbitrary = ModuleDecl <$> listOf1 arbitrary

instance Arbitrary ModuleSegment where
    arbitrary = T.pack <$> moduleSegment
        where
          moduleSegment = 
              (:) <$> elements ['A'..'Z']
                  <*> listOf (elements $ ['a'..'z'] ++
                                         ['0'..'9'] ++ "-_")

instance Stringify ModuleDecl where
    stringify (ModuleDecl segs) = printf "module %s" (str segs)
        where str = T.unpack . T.intercalate "."




