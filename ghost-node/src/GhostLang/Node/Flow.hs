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
    , loadProgram
    , listPatterns
    ) where

import Control.Concurrent.Async (async)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GhostLang ( GhostPattern
                 , PatternTuple
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
                            , allPatterns
                            , modifyTVar'IO
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
      key <- genId
      let url  = "/program/" `T.append` key
          repr = ProgramRepr { programPath_ = programPath
                             , programUrl   = url
                             , ghostProgram = program
                             , patternList  = toPatternList program
                             }
          answer = Resource { resourceUrl = url }
      insertProgram state key repr
      return $ Right answer
    Left err      -> return $ Left err

-- | Run a named pattern from the program identified by the resource
-- key.
runNamedPattern :: State -> ResourceKey -> NamedPattern 
                -> IO (Either ByteString Resource)
runNamedPattern state key namedPattern@NamedPattern {..} = do
  -- A lot of looking things up ...
  maybeProgram <- lookupProgram state key
  case maybeProgram of
    Just program ->
      case fromPatternList execPattern $ patternList program of

        -- Yes, both program and pattern is found.
        Just pattern -> Right <$> runNamedPattern' state namedPattern pattern

        -- Not able to find the requested pattern.
        Nothing      -> return $
            Left ("Not found pattern: " `BS.append` encodeUtf8 execPattern)

    -- Not able to find the requested program.
    Nothing      -> return $ 
       Left ("Not found program key: " `BS.append` encodeUtf8 key)

-- | Help function to run a named pattern. Do the core stuff.
runNamedPattern' :: State -> NamedPattern -> GhostPattern -> IO Resource
runNamedPattern' state NamedPattern {..} pattern = do
  localCounter <- newTVarIO emptyCounter
  networkConf' <- readTVarIO $ networkConf state
  key          <- genId
                                                  
  let networkConf'' = networkConf' { srcIpAddress = srcIp execParams }
      url           = "/pattern/" `T.append` key

  async_' <- async $ runPattern pattern [localCounter, globalCounter state]
                                networkConf'' (shallTrace execParams)
                                (logger state)

  insertPattern state key $ PatternRepr { patternUrl   = url
                                        , ghostPattern = pattern
                                        , async_       = async_' }
  return Resource { resourceUrl = url }

-- | List all patterns instances.
listPatterns :: State -> IO [Resource]
listPatterns state = map (Resource . patternUrl) <$> allPatterns state

-- | Try find a pattern with a specific name from a list of pattern
-- tuples.
fromPatternList :: Text -> [PatternTuple] -> Maybe GhostPattern
fromPatternList label patterns = 
    maybe Nothing lastInTuple $ find matchingLabel patterns
    where 
      matchingLabel (label', _, _) = label' == label
      lastInTuple (_, _, pattern)  = Just pattern
