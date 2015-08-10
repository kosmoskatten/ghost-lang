-- | GhostLang library. Module provide compilation of ghost program
-- from source files, transform of the program to a list of patterns
-- and the ability to run a pattern.
module GhostLang
    ( GhostProgram
    , GhostPattern
    , Counter (..)
    , Mode (..)
    , NetworkConfiguration (..)
    , compileAndLink
    , emptyNetworkConfiguration
    , toPatternList
    , runPattern
    ) where

import GhostLang.Compiler (compileAndLink)
import GhostLang.Interpreter (IntrinsicSet, runPattern')
import GhostLang.Types ( Label
                       , Weight
                       , Program (..)
                       , Pattern (..)
                       )
import GhostLang.RuntimeState ( TVar
                              , Counter (..)
                              , Mode (..)
                              , NetworkConfiguration (..)
                              , emptyNetworkConfiguration )

-- | Convenience type aliases for external usage.
type GhostProgram = Program IntrinsicSet
type GhostPattern = Pattern IntrinsicSet

-- | A pattern presented in a more convenient way for external usage.
type PatternTuple = (Label, Weight, GhostPattern)

-- | Export the program as a pattern list.
toPatternList :: GhostProgram -> [PatternTuple]
toPatternList (Program xs) = map extrTuple xs
    where extrTuple p@(Pattern _ l w _) = (l, w, p)

-- | Run a selected pattern with a set of counters, a network
-- configuration and a runtime mode.
runPattern :: GhostPattern 
           -> [TVar Counter] 
           -> NetworkConfiguration 
           -> Mode 
           -> IO ()
runPattern = runPattern'


