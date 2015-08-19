{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Shell
    ( Shell
    , runShell
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
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import GhostLang.API ( PatternInfo (..)
                     , Resource (..)
                     , loadProgram
                     , listSelectedProgram
                     , listPrograms
                     )
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)

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

nodeLoadProgram :: Text -> Shell (Either String Text)
nodeLoadProgram filePath = do
  baseUrl <- nodeAddress <$> get
  mgr     <- manager     <$> get
  liftIO $ loadProgram mgr baseUrl filePath

nodeListSelectedProgram :: Shell (Either String [PatternInfo])
nodeListSelectedProgram = do
  maybeProg <- progResource <$> get
  maybe (return $ Left "No saved program") nodeListProgram' maybeProg
    where 
      nodeListProgram' :: Text -> Shell (Either String [PatternInfo])
      nodeListProgram' prog = do
          baseUrl <- nodeAddress <$> get
          mgr     <- manager     <$> get
          liftIO $ listSelectedProgram mgr baseUrl prog

nodeListPrograms :: Shell (Either String [Resource])
nodeListPrograms = do
  baseUrl <- nodeAddress <$> get
  mgr     <- manager     <$> get
  liftIO $ listPrograms mgr baseUrl

storeProgramResource :: Text -> Shell ()
storeProgramResource res = modify' $ \s -> s { progResource = Just res }
