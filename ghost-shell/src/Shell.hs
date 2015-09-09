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
    , nodeRunNamedPattern
    , nodeRunRandomPattern
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
import GhostLang.API ( PatternInfo (..)
                     , ProgramPath (..)
                     , Resource (..)
                     , Service (..)
                     , NamedPattern (..)
                     , ExecParams (..)
                     , getHttpConfig
                     , setHttpConfig
                     , loadProgram
                     , listSelectedProgram
                     , listPrograms
                     , runNamedPattern
                     , runRandomPattern
                     )
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import qualified Data.Text as T

data Context = Context { nodeAddress  :: !String
                       , progResource :: !(Maybe Resource)
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

nodeLoadProgram :: FilePath -> Shell (Either String Resource)
nodeLoadProgram filePath = do
  (mgr, baseUrl) <- nodeParams
  liftIO $ loadProgram mgr baseUrl ProgramPath { programPath = T.pack filePath }

nodeListSelectedProgram :: Shell (Either String [PatternInfo])
nodeListSelectedProgram = do
  maybeProg <- progResource <$> get
  maybe (return $ Left "No saved program") nodeListProgram' maybeProg
    where 
      nodeListProgram' :: Resource -> Shell (Either String [PatternInfo])
      nodeListProgram' prog = do
          (mgr, baseUrl) <- nodeParams          
          liftIO $ listSelectedProgram mgr baseUrl prog

nodeListPrograms :: Shell (Either String [Resource])
nodeListPrograms = do
  (mgr, baseUrl) <- nodeParams
  liftIO $ listPrograms mgr baseUrl

nodeRunNamedPattern :: String -> Bool -> Maybe String 
                    -> Shell (Either String Resource)
nodeRunNamedPattern name trace src = do
  maybeProg <- progResource <$> get
  maybe (return $ Left "No saved program") nodeRunNamedPattern' maybeProg
    where
      nodeRunNamedPattern' :: Resource -> Shell (Either String Resource)
      nodeRunNamedPattern' prog = do
          let namedPattern = 
                  NamedPattern { execPattern = T.pack name
                               , execParams  = ExecParams { shallTrace = trace
                                                          , srcIp      = src }
                               }
          (mgr, baseUrl) <- nodeParams
          liftIO $ runNamedPattern mgr baseUrl prog namedPattern

nodeRunRandomPattern :: Bool -> Maybe String -> Shell (Either String Resource)
nodeRunRandomPattern trace src = do
  maybeProg <- progResource <$> get
  maybe (return $ Left "No saved program") nodeRunRandomPattern' maybeProg
    where
      nodeRunRandomPattern' :: Resource -> Shell (Either String Resource)
      nodeRunRandomPattern' prog = do
          let params = ExecParams { shallTrace = trace
                                  , srcIp      = src
                                  }
          (mgr, baseUrl) <- nodeParams
          liftIO $ runRandomPattern mgr baseUrl prog params

storeProgramResource :: Resource -> Shell ()
storeProgramResource res = modify' $ \s -> s { progResource = Just res }

nodeParams :: Shell (Manager, String)
nodeParams = (,) <$> manager' <*> nodeAddress'
    where
      manager'     = manager     <$> get
      nodeAddress' = nodeAddress <$> get
