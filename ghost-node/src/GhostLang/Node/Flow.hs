{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The GhostLang.Node.Flow module is implementing the Flows -
-- business logic - for the Node.
module GhostLang.Node.Flow
    ( getHttpConfig
    , setHttpConfig
    , listPrograms
    , listPatternsFromProgram
    , loadProgram
    ) where

import Control.Concurrent.STM (readTVarIO)
import GhostLang ( compileAndLink
                 , toPatternList 
                 )
import GhostLang.API ( PatternInfo (..)
                     , ProgramPath (..)
                     , Resource (..)
                     , Service (..)
                     )
import GhostLang.Node.IdGen (genId)
import GhostLang.Node.State ( State (..)
                            , ResourceKey
                            , ProgramRepr (..)
                            , NetworkConfiguration (..)
                            , insertProgram
                            , lookupProgram
                            , allPrograms
                            , modifyTVar'IO
                            )
import qualified Data.Text as T

-- | Get the current http configuration.
getHttpConfig :: State -> IO Service
getHttpConfig state = do
  nw <- readTVarIO $ networkConf state
  return $ Service { serviceAddress = httpServiceAddress nw
                   , servicePort    = httpServicePort nw }

-- | Set the http configuration.
setHttpConfig :: State -> Service -> IO ()
setHttpConfig State {..} Service {..} = 
    modifyTVar'IO networkConf $ \nw -> 
        nw { httpServiceAddress = serviceAddress
           , httpServicePort    = servicePort }

-- | List all registered programs.
listPrograms :: State -> IO [Resource]
listPrograms state = map (Resource . resourceId_) <$> allPrograms state

-- | List the patterns from the selected program.
listPatternsFromProgram :: State -> ResourceKey -> IO (Maybe [PatternInfo])
listPatternsFromProgram state key = do
  maybe Nothing (Just . mapPatternInfo) <$> lookupProgram state key
      where mapPatternInfo = map (\(l, w, _) -> PatternInfo l w) . patternList

-- | Compile the ghost program, if succesful store it in the state.
loadProgram :: State -> ProgramPath -> IO (Either String Resource)
loadProgram state ProgramPath {..} = do
  result <- compileAndLink (T.unpack programPath)
  case result of
    Right program -> do
      key <- genId
      let resId  = "/program/" `T.append` key
          repr   = ProgramRepr { programPath_ = programPath
                               , resourceId_  = resId
                               , ghostProgram = program
                               , patternList  = toPatternList program
                               }
          answer = Resource { resourceId = resId }
      insertProgram state key repr
      return $ Right answer
    Left err      -> return $ Left err
