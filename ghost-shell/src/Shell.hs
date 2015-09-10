{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Shell
    ( State
    , mkState
    , nodeGetHttpConfig
    , nodeSetHttpConfig
    , nodeLoadProgram
    , nodeListSelectedProgram
    , nodeListPrograms
    , nodeListPatterns
    , nodeRunNamedPattern
    , nodeRunRandomPattern
    , nodeListGlobalCounter
    , storeProgramResource
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, modifyIORef', readIORef)
import GhostLang.API ( PatternInfo (..)
                     , ProgramPath (..)
                     , Resource (..)
                     , Service (..)
                     , NamedPattern (..)
                     , ExecParams (..)
                     , PatternCounter (..)
                     , getHttpConfig
                     , setHttpConfig
                     , loadProgram
                     , listSelectedProgram
                     , listPrograms
                     , listPatterns
                     , runNamedPattern
                     , runRandomPattern
                     , listGlobalCounter
                     )
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import qualified Data.Text as T

data State = State { nodeAddress  :: !String
                   , progResource :: !(Maybe Resource)
                   , manager      :: !Manager
                   }

mkState :: MonadIO m => String -> m State
mkState nodeAddress' = State nodeAddress' Nothing <$> 
                             liftIO (newManager defaultManagerSettings)

nodeGetHttpConfig :: MonadIO m => IORef State -> m (Either String Service)
nodeGetHttpConfig state = do
  (mgr, baseUrl) <- nodeParams <$> liftIO (readIORef state)
  liftIO $ getHttpConfig mgr baseUrl

nodeSetHttpConfig :: MonadIO m => IORef State -> String -> Int 
                  -> m (Either String ())
nodeSetHttpConfig state server port = do
  (mgr, baseUrl) <- nodeParams <$> liftIO (readIORef state)
  liftIO $ setHttpConfig mgr baseUrl $ Service { serviceAddress = server
                                               , servicePort    = port }

nodeLoadProgram :: MonadIO m => IORef State -> FilePath 
                -> m (Either String Resource)
nodeLoadProgram state filePath = do
  (mgr, baseUrl) <- nodeParams <$> liftIO (readIORef state)
  liftIO $ loadProgram mgr baseUrl ProgramPath { programPath = T.pack filePath }

nodeListSelectedProgram :: MonadIO m => IORef State ->
                           m (Either String [PatternInfo])
nodeListSelectedProgram state = do
  maybeProg <- progResource <$> liftIO (readIORef state)
  maybe (return $ Left "No saved program") nodeListProgram' maybeProg
    where 
      nodeListProgram' :: MonadIO m => Resource 
                       -> m (Either String [PatternInfo])
      nodeListProgram' prog = do
          (mgr, baseUrl) <- nodeParams <$> liftIO (readIORef state)
          liftIO $ listSelectedProgram mgr baseUrl prog

nodeListPrograms :: MonadIO m => IORef State -> m (Either String [Resource])
nodeListPrograms state = do
  (mgr, baseUrl) <- nodeParams <$> liftIO (readIORef state)
  liftIO $ listPrograms mgr baseUrl

nodeListPatterns :: MonadIO m => IORef State -> m (Either String [Resource])
nodeListPatterns state = do
  (mgr, baseUrl) <- nodeParams <$> liftIO (readIORef state)
  liftIO $ listPatterns mgr baseUrl

nodeRunNamedPattern :: MonadIO m => IORef State -> String -> Bool 
                    -> Maybe String -> m (Either String Resource)
nodeRunNamedPattern state name trace src = do
  maybeProg <- progResource <$> liftIO (readIORef state)
  maybe (return $ Left "No saved program") nodeRunNamedPattern' maybeProg
    where
      nodeRunNamedPattern' :: MonadIO m => Resource -> m (Either String Resource)
      nodeRunNamedPattern' prog = do
          let namedPattern = 
                  NamedPattern { execPattern = T.pack name
                               , execParams  = ExecParams { shallTrace = trace
                                                          , srcIp      = src }
                               }
          (mgr, baseUrl) <- nodeParams <$> liftIO (readIORef state)
          liftIO $ runNamedPattern mgr baseUrl prog namedPattern

nodeRunRandomPattern :: MonadIO m => IORef State -> Bool -> Maybe String 
                     -> m (Either String Resource)
nodeRunRandomPattern state trace src = do
  maybeProg <- progResource <$> liftIO (readIORef state)
  maybe (return $ Left "No saved program") nodeRunRandomPattern' maybeProg
    where
      nodeRunRandomPattern' :: MonadIO m => Resource -> m (Either String Resource)
      nodeRunRandomPattern' prog = do
          let params = ExecParams { shallTrace = trace
                                  , srcIp      = src
                                  }
          (mgr, baseUrl) <- nodeParams <$> liftIO (readIORef state)
          liftIO $ runRandomPattern mgr baseUrl prog params

nodeListGlobalCounter :: MonadIO m => IORef State 
                      -> m (Either String PatternCounter)
nodeListGlobalCounter state = do
  (mgr, baseUrl) <- nodeParams <$> liftIO (readIORef state)
  liftIO $ listGlobalCounter mgr baseUrl

storeProgramResource :: MonadIO m => IORef State -> Resource -> m ()
storeProgramResource state res = 
    liftIO $ modifyIORef' state $ \s -> s { progResource = Just res }

nodeParams :: State -> (Manager, String)
nodeParams State {..} = (manager, nodeAddress)
