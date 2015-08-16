{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Shell
    ( Shell
    , runShell
    , liftIO
    ) where

import Control.Monad.Reader ( ReaderT
                            , MonadReader
                            , runReaderT
                            , ask
                            )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromJust, isJust)
import GhostLang.API (loadProgram)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)

data Context = Context { nodeAddress :: !String
                       , manager     :: !Manager }

newtype Shell a = Shell { extractShell :: ReaderT Context IO a }
    deriving (Functor, Applicative, Monad, MonadReader Context, MonadIO)

runShell :: Shell a -> String -> IO a
runShell act address = do
  context <- Context address <$> newManager defaultManagerSettings
  runReaderT (extractShell act) context

nodeLoadProgram :: FilePath -> Shell (Either String ())
nodeLoadProgram filePath = do
  baseUrl <- nodeAddress <$> ask
  mgr     <- manager     <$> ask
  liftIO $ loadProgram mgr baseUrl filePath

