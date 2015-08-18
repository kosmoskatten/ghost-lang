{-# LANGUAGE RecordWildCards #-}
module GhostLang.Node.State
    ( State
    , ProgramRepr (..)
    , emptyState
    , insertProgram
    ) where

import Control.Concurrent.STM ( TVar
                              , atomically
                              , newTVarIO
                              , modifyTVar'
                              )
import GhostLang (GhostProgram, PatternTuple)
import qualified Data.Map.Strict as Map

-- | A representation of a compiled program.
data ProgramRepr =
    ProgramRepr { filePath     :: !FilePath
                , resourceId   :: !String
                , ghostProgram :: !GhostProgram
                , patternList  :: ![PatternTuple]
                }

-- | Map from (program id) string to the corresponding program
-- representation.
type ProgramMap = Map.Map String ProgramRepr

-- | State for the ghost-node.
data State = State { programMap :: TVar ProgramMap }

-- | Create an empty state.
emptyState :: IO State
emptyState = State <$> newTVarIO Map.empty

insertProgram :: State -> String -> ProgramRepr -> IO ()
insertProgram State {..} resId prog = 
    atomically $ modifyTVar' programMap $ Map.insert resId prog

