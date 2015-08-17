module GhostLang.Node.State
    ( State
    , emptyState
    ) where

import Control.Concurrent.STM (TVar, newTVarIO)
import GhostLang (GhostProgram, PatternTuple)
import qualified Data.Map.Strict as Map

-- | A representation of a compiled program.
data ProgramRepr =
    ProgramRepr { ghostProgram :: !GhostProgram
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
