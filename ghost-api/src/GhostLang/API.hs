{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module GhostLang.API
    ( ProgramPath (..)
    , PatternInfo (..)
    , Resource (..)
    , Service (..)
    , ExecParams (..)
    , NamedPattern (..)
    , PatternStatus (..)
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
    , listSelectedCounter
    , listSelectedStatus
    , module Data.Aeson
    ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Network.HTTP.Client ( Manager
                           , Response (..)
                           , Request (..)
                           , RequestBody (..)
                           , httpLbs
                           , parseUrl )
import Network.HTTP.Types
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T

-- | A data type that is describing the file path to the Main module
-- for the program to be compiled and loaded.
data ProgramPath = ProgramPath { programPath :: !Text }
    deriving (Eq, Generic, Show)

-- | A data type that is describing the name and weight for a pattern.
data PatternInfo = PatternInfo { patternName   :: !Text
                               , patternWeight :: !Int64
                               }
    deriving (Eq, Generic, Show)

-- | A data type that is describing a resource url.
data Resource = Resource { resourceUrl :: !Text }
    deriving (Eq, Generic, Show)

-- | A data type that is describing a remote service.
data Service = Service { serviceAddress :: !String
                       , servicePort    :: !Int
                       }
    deriving (Eq, Generic, Show)

-- | A data type that is describing pattern execution parameters.
data ExecParams = ExecParams { shallTrace     :: !Bool
                             , srcIp          :: !(Maybe String)
                             }
    deriving (Eq, Generic, Show)

-- | A data type that is describing the particulars for execution of a
-- named pattern.
data NamedPattern = NamedPattern { execPattern :: !Text
                                 , execParams  :: !ExecParams
                                 }
    deriving (Eq, Generic, Show)

-- | A data type describing if a pattern has completed, failed or if
-- it is having a failure message.
data PatternStatus = PatternStatus { completed   :: !Bool
                                   , failed      :: !Bool
                                   , failMessage :: !Text
                                   }
    deriving (Eq, Generic, Show)

-- | Pattern level counters with information on execution times and
-- data amounts.
data PatternCounter = PatternCounter { totalTimeS   :: !Double
                                     , httpGetTimeS :: !Double
                                     , httpGetBytes :: !Int64
                                     , httpPutTimeS :: !Double
                                     , httpPutBytes :: !Int64
                                     }
    deriving (Eq, Generic, Show)

type Server      = String
type ServerReply = (Status, LBS.ByteString)

-- | Get the http configuration.
getHttpConfig :: Manager -> Server -> IO (Either String Service)
getHttpConfig mgr baseUrl =
  jsonBody status200 =<< (tryString $ do
      req <- mkGetRequest (baseUrl `mappend` httpConfigUrl)
      serverTalk req mgr)

-- | Set the http configuration.
setHttpConfig :: Manager -> Server -> Service -> IO (Either String ())
setHttpConfig mgr baseUrl conf = do
  result <- tryString $ do
    req <- mkPutRequest (baseUrl `mappend` httpConfigUrl) conf
    serverTalk req mgr
  case result of
    Right (stat, body)
        | stat == status200 -> return $ Right ()
        | otherwise         -> return $ Left (LBS.unpack body)
    Left e                  -> return $ Left e

httpConfigUrl :: String
httpConfigUrl = "/configuration/http"

-- | Load a program on the remote node.
loadProgram :: Manager -> Server -> ProgramPath -> IO (Either String Resource)
loadProgram mgr baseUrl prog =
  jsonBody status201 =<< (tryString $ do
      req <- mkPostRequest (baseUrl `mappend` loadProgramUrl) prog
      serverTalk req mgr)

-- | Url to the resource on which a program load can be issued.
loadProgramUrl :: String
loadProgramUrl = "/program/load"

-- | List the patterns for the selected program on the remote node.
listSelectedProgram :: Manager -> Server -> Resource 
                    -> IO (Either String [PatternInfo])
listSelectedProgram mgr baseUrl res =
  jsonBody status200 =<< (tryString $ do
      let url = baseUrl `mappend` (T.unpack $ resourceUrl res) `mappend` "/list"
      req <- mkGetRequest url
      serverTalk req mgr)

-- | List the resource urls for all loaded programs on the node.
listPrograms :: Manager -> Server -> IO (Either String [Resource])
listPrograms mgr baseUrl =
  jsonBody status200 =<< (tryString $ do
      req <- mkGetRequest (baseUrl `mappend` "/program/list")
      serverTalk req mgr)
  
-- | List the resource urls for all the in-flight patterns on the
-- node.
listPatterns :: Manager -> Server -> IO (Either String [Resource])
listPatterns mgr baseUrl =
  jsonBody status200 =<< (tryString $ do
    req <- mkGetRequest (baseUrl `mappend` "/pattern/list")
    serverTalk req mgr)

-- | Run a named pattern from the program described by the
-- resource. If successful a resource to the pattern is returned.
runNamedPattern :: Manager -> Server -> Resource 
                -> NamedPattern -> IO (Either String Resource)
runNamedPattern mgr baseUrl res pattern =
  jsonBody status201 =<< (tryString $ do
      let url = baseUrl `mappend` (T.unpack $ resourceUrl res) 
                        `mappend` "/named-pattern"
      req <- mkPostRequest url pattern
      serverTalk req mgr)

-- | Run a random pattern from the program described by the
-- resource. If successful a resource to the pattern is returned.
runRandomPattern :: Manager -> Server -> Resource
                 -> ExecParams -> IO (Either String Resource)
runRandomPattern mgr baseUrl res params = do
  jsonBody status201 =<< (tryString $ do
      let url = baseUrl `mappend` (T.unpack $ resourceUrl res)
                        `mappend` "/random-pattern"
      req <- mkPostRequest url params
      serverTalk req mgr)

-- | List the global pattern counter.
listGlobalCounter :: Manager -> Server -> IO (Either String PatternCounter)
listGlobalCounter mgr baseUrl =
  jsonBody status200 =<< (tryString $ do
    req <- mkGetRequest (baseUrl `mappend` "/pattern/counter")
    serverTalk req mgr)

-- | List the counter for a selected pattern.
listSelectedCounter :: Manager -> Server -> Resource 
                    -> IO (Either String PatternCounter)
listSelectedCounter mgr baseUrl res =
  jsonBody status200 =<< (tryString $ do
    let url = baseUrl `mappend` (T.unpack $ resourceUrl res)
                      `mappend` "/counter"
    req <- mkGetRequest url
    serverTalk req mgr)

-- | List the status for a selected pattern.
listSelectedStatus :: Manager -> Server -> Resource 
                   -> IO (Either String PatternStatus)
listSelectedStatus mgr baseUrl res = do
  let url = baseUrl `mappend` (T.unpack $ resourceUrl res) `mappend` "/status"
  jsonGet mgr url

-- | Make a POST request carrying a JSON object and expecting a JSON
-- object as response. Can throw exception if the url is malformed.
mkPostRequest :: ToJSON a => String -> a -> IO Request
mkPostRequest url msg = do
  initReq <- parseUrl url
  let req = initReq
            { method         = "POST"
            , requestHeaders = [ ("Content-Type", "application/json")
                               , ("Accept", "application/json") ]
            , checkStatus    = \_ _ _ -> Nothing
            , requestBody    = RequestBodyLBS (encode msg)
            }
  return req

-- | Make a GET request and expecting a JSON object as response. Can
-- throw exception if the url is malformed.
mkGetRequest :: String -> IO Request
mkGetRequest url = do
  initReq <- parseUrl url
  let req = initReq
            { requestHeaders = [ ("Accept", "application/json") ]
            , checkStatus    = \_ _ _ -> Nothing
            }
  return req

-- | Make PUT request carrying a JSON object and expecting a non
-- payload object as response.
mkPutRequest :: ToJSON a => String -> a -> IO Request
mkPutRequest url msg = do
  initReq <- parseUrl url
  let req = initReq
            { method      = "PUT"
            , checkStatus = \_ _ _ -> Nothing
            , requestBody = RequestBodyLBS (encode msg)
            }
  return req

-- | Send a request and expect and wait for the reply. Can throw
-- exceptions.
serverTalk :: Request -> Manager -> IO ServerReply
serverTalk req mgr = do
  resp <- httpLbs req mgr
  return (responseStatus resp, responseBody resp)

jsonGet :: FromJSON a => Manager -> String -> IO (Either String a)
jsonGet mgr url = do
  jsonBody status200 =<< (tryString $ do
    req <- mkGetRequest url
    serverTalk req mgr)

jsonBody :: FromJSON a => Status -> Either String ServerReply 
         -> IO (Either String a)
jsonBody accept reply =
    case reply of
      Right (stat, body)
          | stat == accept -> return $ Right (strongDecode body)
          | otherwise      -> return $ Left (LBS.unpack body)
      Left err             -> return $ Left err

strongDecode :: FromJSON a => LBS.ByteString -> a
strongDecode = fromJust . decode

-- | Evaluate an action, if it fails with an exception convert the
-- exception to a string.
tryString :: IO a -> IO (Either String a)
tryString act = do
  res <- try act
  case res of
    Left e  -> return (Left $ toString e)
    Right r -> return (Right r)
    where
      toString :: SomeException -> String
      toString e = show e

-- | Aeson instances.
instance FromJSON ProgramPath
instance ToJSON ProgramPath
instance FromJSON PatternInfo
instance ToJSON PatternInfo
instance FromJSON Resource
instance ToJSON Resource
instance FromJSON Service
instance ToJSON Service
instance FromJSON ExecParams
instance ToJSON ExecParams
instance FromJSON NamedPattern
instance ToJSON NamedPattern
instance FromJSON PatternStatus
instance ToJSON PatternStatus
instance FromJSON PatternCounter
instance ToJSON PatternCounter
