{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The GhostLang.Node.Flow module is implementing the Flows -
-- business logic - for the Node.
module GhostLang.Node.Flow
    ( getHttpConfig
    , setHttpConfig
    , listPrograms
    , listPatternsFromProgram
    , runNamedPattern
    , runRandomPattern
    , loadProgram
    , listPatterns
    , getGlobalCounter
    , getPatternCounter
    , patternStatus
    ) where

import Control.Concurrent.Async (async, poll)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GhostLang ( GhostPattern
                 , PatternTuple
                 , Counter (..)
                 , compileAndLink
                 , emptyCounter
                 , runPattern
                 , toPatternList 
                 )
import GhostLang.API ( PatternInfo (..)
                     , ProgramPath (..)
                     , Resource (..)
                     , Service (..)
                     , ExecParams (..)
                     , NamedPattern (..)
                     , PatternStatus (..)
                     , PatternCounter (..)
                     )
import GhostLang.Node.IdGen (genId)
import GhostLang.Node.State ( State (..)
                            , ResourceKey
                            , ProgramRepr (..)
                            , PatternRepr (..)
                            , NetworkConfiguration (..)
                            , insertProgram
                            , lookupProgram
                            , allPrograms
                            , insertPattern
                            , lookupPattern
                            , allPatterns
                            , modifyTVar'IO
                            , mkRandomSelector
                            )
import qualified Data.ByteString.Char8 as BS
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
listPrograms state = map (Resource . programUrl) <$> allPrograms state

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
      key            <- genId
      randomPattern' <- mkRandomSelector $ toPatternList program
      let url  = "/program/" `T.append` key
          repr = ProgramRepr { programPath_  = programPath
                             , programUrl    = url
                             , ghostProgram  = program
                             , patternList   = toPatternList program
                             , randomPattern = randomPattern'
                             }
          answer = Resource { resourceUrl = url }
      insertProgram state key repr
      return $ Right answer
    Left err      -> return $ Left err

-- | Run a named pattern from the program identified by the resource
-- key.
runNamedPattern :: State -> ResourceKey -> NamedPattern 
                -> IO (Either ByteString Resource)
runNamedPattern state key NamedPattern {..} = do
  -- A lot of looking things up ...
  maybeProgram <- lookupProgram state key
  case maybeProgram of
    Just program ->
      case fromPatternList execPattern $ patternList program of

        -- Yes, both program and pattern is found.
        Just pattern -> Right <$> runSelectedPattern state execParams pattern

        -- Not able to find the requested pattern.
        Nothing      -> return $
            Left ("Not found pattern: " `BS.append` encodeUtf8 execPattern)

    -- Not able to find the requested program.
    Nothing      -> return $ 
      Left ("Not found program key: " `BS.append` encodeUtf8 key)

-- | Run a random pattern from the program identified by the resource
-- key.
runRandomPattern :: State -> ResourceKey -> ExecParams 
                 -> IO (Either ByteString Resource)
runRandomPattern state key params = do
  maybeProgram <- lookupProgram state key
  case maybeProgram of
    Just program -> do
      pattern <- randomPattern program
      Right <$> runSelectedPattern state params pattern
    Nothing      -> return $
      Left ("Not found program key: " `BS.append` encodeUtf8 key)

-- | Help function to run a selected pattern. Do the core stuff.
runSelectedPattern :: State -> ExecParams -> GhostPattern -> IO Resource
runSelectedPattern state ExecParams {..} pattern = do
  localCounter' <- newTVarIO emptyCounter
  networkConf'  <- readTVarIO $ networkConf state
  key           <- genId
                                                  
  let networkConf'' = networkConf' { srcIpAddress = srcIp }
      url           = "/pattern/" `T.append` key

  async_' <- async $ runPattern pattern [localCounter', globalCounter state]
                                networkConf'' 
                                (dataChunk state)
                                shallTrace
                                (logger state)

  insertPattern state key $ PatternRepr { patternUrl   = url
                                        , ghostPattern = pattern
                                        , localCounter = localCounter'
                                        , async_       = async_' }
  return Resource { resourceUrl = url }

-- | List all patterns instances.
listPatterns :: State -> IO [Resource]
listPatterns state = map (Resource . patternUrl) <$> allPatterns state

-- | List the global counter.
getGlobalCounter :: State -> IO PatternCounter
getGlobalCounter state = fromCounter <$> readTVarIO (globalCounter state)

-- | List the counter from a selected pattern.
getPatternCounter :: State -> ResourceKey -> IO (Maybe PatternCounter)
getPatternCounter state key = 
  maybe (return Nothing) fromCounter' =<< lookupPattern state key
      where 
        fromCounter' :: PatternRepr -> IO (Maybe PatternCounter)
        fromCounter' p = Just . fromCounter <$> (readTVarIO $ localCounter p)

-- | List pattern execution status.
patternStatus :: State -> ResourceKey -> IO (Maybe PatternStatus)
patternStatus state key = do
  maybe (return Nothing) patternStatus' =<< lookupPattern state key
    where
      patternStatus' :: PatternRepr -> IO (Maybe PatternStatus)
      patternStatus' PatternRepr {..} = do
        result <- poll async_
        case result of
          Just status ->
              case status of
                Right () ->
                  return $ Just PatternStatus { completed   = True
                                              , failed      = False
                                              , failMessage = "" }
                Left e   ->
                  return $ Just PatternStatus { completed   = True
                                              , failed      = True
                                              , failMessage = T.pack $ show e }
          Nothing     ->
            return $ Just PatternStatus { completed   = False
                                        , failed      = False
                                        , failMessage = "" }

-- | Try find a pattern with a specific name from a list of pattern
-- tuples.
fromPatternList :: Text -> [PatternTuple] -> Maybe GhostPattern
fromPatternList label patterns = 
    maybe Nothing lastInTuple $ find matchingLabel patterns
    where 
      matchingLabel (label', _, _) = label' == label
      lastInTuple (_, _, pattern)  = Just pattern

-- | Convert from the internal counter format
fromCounter :: Counter -> PatternCounter
fromCounter Counter {..} =
    PatternCounter { totalTimeS    = realToFrac patternExecTime
                   , httpGetTimeS  = realToFrac httpGETExecTime
                   , httpGetBytes  = httpGETBytes
                   , httpPutTimeS  = realToFrac httpPUTExecTime
                   , httpPutBytes  = httpPUTBytes
                   }
