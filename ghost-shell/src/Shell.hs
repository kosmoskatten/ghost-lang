{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Shell
    ( Shell
    , runShell
    , nodeGetHttpConfig
    , nodeSetHttpConfig
    , nodeLoadProgram
    , nodeListSelectedProgram
    , nodeListPrograms
    , storeProgramResource
    , liftIO
    ) where

import Control.Monad.State ( StateT
                           , MonadState
                           , evalStateT
                           , get
                           , modify'
                           )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import GhostLang.API ( PatternInfo (..)
                     , Resource (..)
                     , Service (..)
                     , getHttpConfig
                     , setHttpConfig
                     , loadProgram
                     , listSelectedProgram
                     , listPrograms
                     )
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import qualified Data.Text as T

data Context = Context { nodeAddress  :: !String
                       , progResource :: !(Maybe Text)
                       , manager      :: !Manager
                       }

newtype Shell a = Shell { extractShell :: StateT Context IO a }
    deriving (Functor, Applicative, Monad, MonadState Context, MonadIO)

runShell :: Shell a -> String -> IO a
runShell act address = do
  context <- Context address Nothing <$> newManager defaultManagerSettings
  evalStateT (extractShell act) context

nodeGetHttpConfig :: Shell (Either String Service)
nodeGetHttpConfig = do
  (mgr, baseUrl) <- nodeParams
  liftIO $ getHttpConfig mgr baseUrl

nodeSetHttpConfig :: String -> Int -> Shell (Either String ())
nodeSetHttpConfig server port = do
  (mgr, baseUrl) <- nodeParams
  liftIO $ setHttpConfig mgr baseUrl $ Service { serviceAddress = server
                                               , servicePort    = port }

nodeLoadProgram :: FilePath -> Shell (Either String Text)
nodeLoadProgram filePath = do
  (mgr, baseUrl) <- nodeParams
  liftIO $ loadProgram mgr baseUrl (T.pack filePath)

nodeListSelectedProgram :: Shell (Either String [PatternInfo])
nodeListSelectedProgram = do
  maybeProg <- progResource <$> get
  maybe (return $ Left "No saved program") nodeListProgram' maybeProg
    where 
      nodeListProgram' :: Text -> Shell (Either String [PatternInfo])
      nodeListProgram' prog = do
          (mgr, baseUrl) <- nodeParams          
          liftIO $ listSelectedProgram mgr baseUrl prog

nodeListPrograms :: Shell (Either String [Resource])
nodeListPrograms = do
  (mgr, baseUrl) <- nodeParams
  liftIO $ listPrograms mgr baseUrl

storeProgramResource :: Text -> Shell ()
storeProgramResource res = modify' $ \s -> s { progResource = Just res }

nodeParams :: Shell (Manager, String)
nodeParams = (,) <$> manager' <*> nodeAddress'
    where
      manager'     = manager     <$> get
      nodeAddress' = nodeAddress <$> get
