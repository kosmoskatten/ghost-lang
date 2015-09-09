{-# LANGUAGE RecordWildCards #-}
module GhostLang.Node.State
    ( State (..)
    , ResourceKey
    , ProgramRepr (..)
    , PatternRepr (..)
    , NetworkConfiguration (..)
    , initState
    , insertProgram
    , lookupProgram
    , insertPattern
    , lookupPattern
    , allPrograms
    , allPatterns
    , modifyTVar'IO
    , mkRandomSelector
    ) where

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM ( TVar
                              , atomically
                              , newTVarIO
                              , modifyTVar'
                              , readTVarIO
                              )
import Data.Text (Text)
import Data.Vector (Vector, (!), fromList)
import GhostLang ( GhostProgram
                 , GhostPattern
                 , PatternTuple
                 , Counter
                 , NetworkConfiguration (..)
                 , emptyCounter
                 , emptyNetworkConfiguration 
                 )
import GhostLang.Conduit (DataChunk, genDataChunk)
import GhostLang.GLog (GLog, newStdoutGLog)
import System.Random.MWC (GenIO, createSystemRandom, uniformR)

import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector

type ResourceKey = Text

-- | A representation of a compiled program.
data ProgramRepr =
    ProgramRepr { programPath_  :: !Text
                , programUrl    :: !Text
                , ghostProgram  :: !GhostProgram
                , patternList   :: ![PatternTuple]
                , randomPattern :: IO GhostPattern
                }

-- | Map from (program id) string to the corresponding program
-- representation.
type ProgramMap = Map.Map ResourceKey ProgramRepr

-- | A representation of a pattern run instance.
data PatternRepr =
    PatternRepr { patternUrl   :: !Text
                , ghostPattern :: !GhostPattern 
                , localCounter :: TVar Counter
                , async_       :: Async ()
                }

-- | Map from (pattern id) string to the corresponding pattern
-- instance representation.
type PatternMap = Map.Map ResourceKey PatternRepr

-- | State for the ghost-node.
data State = State { programMap    :: TVar ProgramMap
                   , patternMap    :: TVar PatternMap
                   , networkConf   :: TVar NetworkConfiguration
                   , globalCounter :: TVar Counter
                   , dataChunk     :: !DataChunk
                   , logger        :: !GLog }

-- | Initialize the state.
initState :: IO State
initState = State <$> newTVarIO Map.empty
                  <*> newTVarIO Map.empty
                  <*> newTVarIO emptyNetworkConfiguration
                  <*> newTVarIO emptyCounter
                  <*> genDataChunk 32768
                  <*> newStdoutGLog

-- | Store a compiled ghost-program into the program map.
insertProgram :: State -> ResourceKey -> ProgramRepr -> IO ()
insertProgram State {..} resId prog = 
    modifyTVar'IO programMap $ Map.insert resId prog

-- | Lookup a ghost-program from the program map.
lookupProgram :: State -> ResourceKey -> IO (Maybe ProgramRepr)
lookupProgram State {..} resId = Map.lookup resId <$> readTVarIO programMap

-- | Enumerate all programs from the program map.
allPrograms :: State -> IO [ProgramRepr]
allPrograms State {..} = Map.elems <$> readTVarIO programMap

-- | Store a pattern instance into the pattern map.
insertPattern :: State -> ResourceKey -> PatternRepr -> IO ()
insertPattern State {..} resId pattern =
    modifyTVar'IO patternMap $ Map.insert resId pattern

-- | Lookup a pattern instance from the pattern map.
lookupPattern :: State -> ResourceKey -> IO (Maybe PatternRepr)
lookupPattern State {..} resId = Map.lookup resId <$> readTVarIO patternMap

-- | Enumerate all patterns from the pattern map.
allPatterns :: State -> IO [PatternRepr]
allPatterns State {..} = Map.elems <$> readTVarIO patternMap

modifyTVar'IO :: TVar a -> (a -> a) -> IO ()
modifyTVar'IO tvar g = atomically $ modifyTVar' tvar g

-- | Expand a list of pattern tuples to a list of ghost patterns. Each
-- pattern is expanded accordingly to its weight.
expandPatterns :: [PatternTuple] -> [GhostPattern]
expandPatterns = concatMap (\(_, w, p) -> replicate (fromIntegral w) p)

-- | Make a random selector for a pattern from the patterns in the
-- list. The probability for a selection is equal to the pattern's
-- weight.
mkRandomSelector :: [PatternTuple] -> IO (IO GhostPattern)
mkRandomSelector xs = do
  random <- createSystemRandom
  let vector = fromList $ expandPatterns xs
      range  = (0, Vector.length vector - 1)
  return $ randomSelector random vector range

randomSelector :: GenIO -> Vector GhostPattern -> (Int, Int) -> IO GhostPattern
randomSelector random vector range = (vector !) <$> uniformR range random
