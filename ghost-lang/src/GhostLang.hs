-- | GhostLang library. Module provide compilation of ghost program
-- from source files, transform of the program to a list of patterns
-- and the ability to run a pattern.
module GhostLang
    ( GhostProgram
    , GhostPattern
    , PatternTuple
    , Counter (..)
    , NetworkConfiguration (..)
    , compileAndLink
    , emptyCounter
    , emptyNetworkConfiguration
    , toPatternList
    , runPattern
    ) where

import GhostLang.Compiler (compileAndLink)
import GhostLang.Conduit (DataChunk)
import GhostLang.Interpreter (IntrinsicSet, runPattern')
import GhostLang.Types ( Label
                       , Weight
                       , Program (..)
                       , Pattern (..)
                       )
import GhostLang.RuntimeState ( RuntimeState (..)
                              , TVar
                              , Counter (..)
                              , GLog
                              , NetworkConfiguration (..)
                              , emptyCounter
                              , emptyNetworkConfiguration )
import GhostLang.Interpreter.Random (createSystemRandom)
import Network.HTTP.Client ( ManagerSettings (..)
                           , defaultManagerSettings
                           , newManager
                           , rawConnectionModifySocket
                           )
import Network.Socket (Socket)

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
-- configuration, payload data, tracing configuration and the logger.
runPattern :: GhostPattern 
           -> [TVar Counter] 
           -> NetworkConfiguration
           -> DataChunk
           -> Bool
           -> GLog
           -> IO ()
runPattern pattern counters' nwConf dataChunk' traceConf glog = do
  mgr     <- newManager managerSettings
  random' <- createSystemRandom
  let state = RuntimeState { counters             = counters'
                           , networkConfiguration = nwConf
                           , connectionMgr        = mgr
                           , dataChunk            = dataChunk'
                           , shallTrace           = traceConf
                           , logger               = glog
                           , random               = random'
                           }
  runPattern' pattern state

managerSettings :: ManagerSettings
managerSettings = 
    defaultManagerSettings 
        { managerRawConnection = rawConnectionModifySocket modifySocket }

modifySocket :: Socket -> IO ()
modifySocket _ = putStrLn "MODIFY"


