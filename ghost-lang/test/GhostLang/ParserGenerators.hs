{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhostLang.ParserGenerators where

import Control.Monad (forM_)
import Control.Monad.Writer (execWriter, tell)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import GhostLang.CommonGenerators ()
import GhostLang.Interpreter (IntrinsicSet (..))
import GhostLang.Types ( Label
                       , GhostModule (..)
                       , ModuleDecl (..)
                       , ImportDecl (..)
                       , TimeUnit (..)
                       , Payload (..)
                       , Pace (..)
                       , Method (..)
                       , Content (..)
                       , Value (..)
                       , Pattern (..)
                       , Procedure (..)
                       , Operation (..)
                       )
import Test.QuickCheck
import Text.Printf (printf)
import qualified Data.Text as T

-- | Typeclass to "stringify", i.e. convert a data structure to real
-- syntax.
class Stringify a where
    stringify :: a -> String

instance Stringify Label where
    stringify = T.unpack

instance Stringify a => Stringify (GhostModule a) where
    stringify (GhostModule modDecl impDecls patterns procs) =
        execWriter $ do
          tell $ printf "%s\n\n" (stringify modDecl)
          forM_ impDecls $ \impDecl ->
              tell $ printf "%s\n" (stringify impDecl)
          forM_ patterns $ \pattern ->
              tell $ printf "%s\n" (stringify pattern)
          forM_ procs $ \proc ->
              tell $ printf "%s\n" (stringify proc)

instance Stringify ModuleDecl where
    stringify (ModuleDecl _ segs) = printf "module %s" (str segs)
        where str = intercalate "."

instance Stringify ImportDecl where
    stringify (ImportDecl segs) = printf "import %s" (str segs)
        where str = intercalate "."

instance Stringify Value where
    stringify (Literal v)      = printf "literal(%ld)" v
    stringify (Stored v)       = printf "%s" (T.unpack v)
    stringify (Gaussian v1 v2) = printf "gaussian(%ld, %ld)" v1 v2
    stringify (Uniform v1 v2)  = printf "uniform(%ld, %ld)" v1 v2

instance Stringify TimeUnit where
    stringify (USec v) = printf "%s usec" (stringify v)
    stringify (MSec v) = printf "%s msec" (stringify v)
    stringify (Sec v)  = printf "%s sec"  (stringify v)

instance Stringify Payload where
    stringify (B v)  = printf "%s B"  (stringify v)
    stringify (KB v) = printf "%s KB" (stringify v)
    stringify (MB v) = printf "%s MB" (stringify v)
    stringify (GB v) = printf "%s GB" (stringify v)

instance Stringify Pace where
    stringify (Bps v)  = printf "%s bps"  (stringify v)
    stringify (Kbps v) = printf "%s kbps" (stringify v)
    stringify (Mbps v) = printf "%s mbps" (stringify v)
    stringify (Gbps v) = printf "%s gbps" (stringify v)

instance Stringify Method where
    stringify m = show m

instance Stringify Content where
    stringify c = map toLower $ show c

instance Stringify IntrinsicSet where
    stringify (Delay t) = printf "Delay %s" (stringify t)
    stringify (Http m cs s p) =
        let str = printf "Http %s [%s] %s" (stringify m) (toCommaStr cs)
                                           (stringify s)
        in if isJust p then str ++ printf " %s" (stringify $ fromJust p)
           else str

instance Stringify a => Stringify (Pattern a) where
    stringify (Pattern _ n w ops) =
        printf "pattern %s with weight %ld { %s }" 
               (T.unpack n) w (stringify ops)

instance Stringify a => Stringify (Procedure a) where
    stringify (Procedure n ps ops) = 
        printf "procedure %s(%s) { %s }" (T.unpack n) 
                                         (toCommaStr ps) 
                                         (stringify ops)

instance Stringify a => Stringify (Operation a) where
    stringify (Invoke i)          = stringify i
    stringify (Loop c is)         = printf "loop %s { %s }" (stringify c) 
                                                            (stringify is)
    stringify (Concurrently is)   = printf "concurrently { %s }" (stringify is)
    stringify (Unresolved _ n vs) = printf "%s (%s)" (T.unpack n) 
                                                     (toCommaStr vs)
    stringify (Call _ _)          = error "Not used"

instance Stringify a => Stringify [a] where
    stringify [] = ""
    stringify xs =
        let y:ys = map stringify xs
            ys'  = map (',':) ys
        in unlines (y:ys')

toCommaStr :: Stringify a => [a] -> String
toCommaStr [] = ""
toCommaStr xs =
    let y:ys = map stringify xs
        ys'  = concatMap (',':) ys
    in y ++ ys'
