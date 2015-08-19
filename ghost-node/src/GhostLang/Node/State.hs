{-# LANGUAGE RecordWildCards #-}
module GhostLang.Node.State
    ( State
    , ProgramRepr (..)
    , emptyState
    , insertProgram
    , lookupProgram
    ) where

import Control.Concurrent.STM ( TVar
                              , atomically
                              , newTVarIO
                              , modifyTVar'
                              , readTVarIO
                              )
import Data.Text (Text)
import GhostLang (GhostProgram, PatternTuple)
import qualified Data.Map.Strict as Map

-- | A representation of a compiled program.
data ProgramRepr =
    ProgramRepr { filePath_    :: !Text
                , resourceId_  :: !Text
                , ghostProgram :: !GhostProgram
                , patternList  :: ![PatternTuple]
                }

-- | Map from (program id) string to the corresponding program
-- representation.
type ProgramMap = Map.Map Text ProgramRepr

-- | State for the ghost-node.
data State = State { programMap :: TVar ProgramMap }

-- | Create an empty state.
emptyState :: IO State
emptyState = State <$> newTVarIO Map.empty

-- | Store a compiled ghost-program into the program map.
insertProgram :: State -> Text -> ProgramRepr -> IO ()
insertProgram State {..} resId prog = 
    atomically $ modifyTVar' programMap $ Map.insert resId prog

-- | Lookup a ghost-program from the program map.
lookupProgram :: State -> Text -> IO (Maybe ProgramRepr)
lookupProgram State {..} resId = Map.lookup resId <$> readTVarIO programMap
