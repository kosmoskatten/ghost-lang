{-# LANGUAGE RecordWildCards #-}
module GhostLang.Node.State
    ( State
    , ProgramRepr (..)
    , NetworkConfiguration (..)
    , emptyState
    , insertProgram
    , lookupProgram
    , allPrograms
    , getHttpConf
    , setHttpConf
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
import GhostLang.API (Service (..))
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
data State = State { programMap  :: TVar ProgramMap
                   , networkConf :: TVar NetworkConfiguration }

-- | Create an empty state.
emptyState :: IO State
emptyState = State <$> newTVarIO Map.empty 
                   <*> newTVarIO emptyNetworkConfiguration

-- | Store a compiled ghost-program into the program map.
insertProgram :: State -> Text -> ProgramRepr -> IO ()
insertProgram State {..} resId prog = 
    modifyTVar'IO programMap $ Map.insert resId prog

-- | Lookup a ghost-program from the program map.
lookupProgram :: State -> Text -> IO (Maybe ProgramRepr)
lookupProgram State {..} resId = Map.lookup resId <$> readTVarIO programMap

-- | Enumerate all programs from the program map.
allPrograms :: State -> IO [ProgramRepr]
allPrograms State {..} = Map.elems <$> readTVarIO programMap

-- | Get the current http configuration.
getHttpConf :: State -> IO Service
getHttpConf state = do
  nw <- readTVarIO $ networkConf state
  return $ Service { serviceAddress = httpServiceAddress nw
                   , servicePort    = httpServicePort nw }

-- | Set the http configuration.
setHttpConf :: State -> Service -> IO ()
setHttpConf State {..} Service {..} = 
    modifyTVar'IO networkConf $ \nw -> nw { httpServiceAddress = serviceAddress
                                          , httpServicePort    = servicePort }

modifyTVar'IO :: TVar a -> (a -> a) -> IO ()
modifyTVar'IO tvar g = atomically $ modifyTVar' tvar g
