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
    , modifyTVar'IO
    ) where

import Control.Concurrent.STM ( TVar
                              , atomically
                              , newTVarIO
                              , modifyTVar'
                              , readTVarIO
                              )
import Data.Text (Text)
import GhostLang ( GhostProgram
                 , PatternTuple
                 , NetworkConfiguration (..)
                 , emptyNetworkConfiguration )
import System.Log.FastLogger ( LoggerSet
                             , defaultBufSize
                             , newStdoutLoggerSet
                             )

import qualified Data.Map.Strict as Map

type ResourceKey = Text

-- | A representation of a compiled program.
data ProgramRepr =
    ProgramRepr { programPath_ :: !Text
                , resourceId_  :: !Text
                , ghostProgram :: !GhostProgram
                , patternList  :: ![PatternTuple]
                }

-- | Map from (program id) string to the corresponding program
-- representation.
type ProgramMap = Map.Map ResourceKey ProgramRepr

-- | A representation of a pattern run instance.
data PatternRepr =
    PatternRepr { resourceIdPattern_ :: !Text }

-- | Map from (pattern id) string to the corresponding pattern
-- instance representation.
type PatternMap = Map.Map ResourceKey PatternRepr

-- | State for the ghost-node.
data State = State { programMap  :: TVar ProgramMap
                   , patternMap  :: TVar PatternMap
                   , networkConf :: TVar NetworkConfiguration
                   , logger      :: !LoggerSet }

-- | Initialize the state.
initState :: IO State
initState = State <$> newTVarIO Map.empty
                  <*> newTVarIO Map.empty
                  <*> newTVarIO emptyNetworkConfiguration
                  <*> newStdoutLoggerSet defaultBufSize

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

modifyTVar'IO :: TVar a -> (a -> a) -> IO ()
modifyTVar'IO tvar g = atomically $ modifyTVar' tvar g
