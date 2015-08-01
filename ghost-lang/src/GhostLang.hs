module GhostLang
    ( GhostProgram
    , GhostPattern
    , compileAndLink
    , toPatternList
    ) where

import GhostLang.Compiler (compileAndLink)
import GhostLang.Intrinsic (IntrinsicSet)
import GhostLang.Types ( Label
                       , Weight
                       , Program (..)
                       , Pattern (..)
                       )

-- | Convenience type aliases for external usage.
type GhostProgram = Program IntrinsicSet
type GhostPattern = Pattern IntrinsicSet

-- | A pattern presented in a more convenient way for external usage.
type PatternTuple = (Label, Weight, GhostPattern)

-- | Export the program as a pattern list.
toPatternList :: GhostProgram -> [PatternTuple]
toPatternList (Program xs) = map extrTuple xs
    where extrTuple p@(Pattern _ l w _) = (l, w, p)
