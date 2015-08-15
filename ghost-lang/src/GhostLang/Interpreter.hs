module GhostLang.Interpreter
    ( IntrinsicSet (..)
    , runPattern'
    ) where

import GhostLang.Interpreter.InterpreterM
import GhostLang.Interpreter.Intrinsic
import GhostLang.Interpreter.InstructionSet
import GhostLang.RuntimeState ( RuntimeState (..)
                              , TVar
                              , Counter                                
                              , Mode
                              , NetworkConfiguration )
import GhostLang.Types (Pattern)
import Network.HTTP.Client ( ManagerSettings (..)
                           , defaultManagerSettings
                           , newManager
                           , rawConnectionModifySocket
                           )
import Network.Socket (Socket)

-- | Run a selected pattern with a set of counters, a network
-- configuration and a runtime mode.
runPattern' :: InstructionSet a 
            => Pattern a 
            -> [TVar Counter] 
            -> NetworkConfiguration
            -> Mode
            -> IO ()
runPattern' p cs nw m = do
  mgr <- newManager managerSettings
  let state = RuntimeState { counters             = cs
                           , networkConfiguration = nw
                           , connectionMgr        = mgr
                           , mode                 = m }
  runInterpreter state $ execPattern p

managerSettings :: ManagerSettings
managerSettings = 
    defaultManagerSettings 
        { managerRawConnection = rawConnectionModifySocket modifySocket }

modifySocket :: Socket -> IO ()
modifySocket _ = putStrLn "MODIFY"
