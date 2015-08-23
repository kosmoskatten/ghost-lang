{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module GhostLang.API
    ( ProgramPath (..)
    , PatternInfo (..)
    , Resource (..)
    , Service (..)
    , getHttpConfig
    , setHttpConfig
    , loadProgram
    , listSelectedProgram
    , listPrograms
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
    deriving (Generic, Show)

-- | A data type that is describing the name and weight for a pattern.
data PatternInfo = PatternInfo { patternName   :: !Text
                               , patternWeight :: !Int64
                               }
    deriving (Generic, Show)

-- | A data type that is describing a resource id (url).
data Resource = Resource { resourceId :: !Text }
    deriving (Generic, Show)

-- | A data type that is describing a remote service.
data Service = Service { serviceAddress :: !String
                       , servicePort    :: !Int
                       }
    deriving (Generic, Show)

type Server = String

-- | Get the http configuration.
getHttpConfig :: Manager -> Server -> IO (Either String Service)
getHttpConfig mgr baseUrl = do
  result <- tryString $ do
      req <- mkGetRequest (baseUrl `mappend` httpConfigUrl)
      serverTalk req mgr
  case result of
    Right (stat, body)
        | stat == status200 -> return $ Right (strongDecode body)
        | otherwise         -> return $ Left (LBS.unpack body)
    Left e                  -> return $ Left e

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
loadProgram mgr baseUrl prog = do
  result <- tryString $ do
      req <- mkPostRequest (baseUrl `mappend` loadProgramUrl) prog
      serverTalk req mgr
  case result of
    Right (stat, body)
        | stat == status201 -> return $ Right (strongDecode body)
        | otherwise         -> return $ Left (LBS.unpack body)
    Left e                  -> return $ Left e

-- | Url to the resource on which a program load can be issued.
loadProgramUrl :: String
loadProgramUrl = "/program/load"

-- | List the patterns for the selected program on the remote node.
listSelectedProgram :: Manager -> Server -> Resource 
                    -> IO (Either String [PatternInfo])
listSelectedProgram mgr baseUrl res = do
  let url = baseUrl `mappend` (T.unpack $ resourceId res) `mappend` "/list"
  result <- tryString $ do
      req <- mkGetRequest url
      serverTalk req mgr
  case result of
    Right (stat, body)
        | stat == status200 -> return $ Right (strongDecode body)
        | otherwise         -> return $ Left (LBS.unpack body)
    Left e                  -> return $ Left e

-- | List the resource urls for all loaded programs on the node.
listPrograms :: Manager -> Server -> IO (Either String [Resource])
listPrograms mgr baseUrl = do
  let url = baseUrl `mappend` listProgramsUrl
  result <- tryString $ do
      req <- mkGetRequest url
      serverTalk req mgr
  case result of
    Right (stat, body)
        | stat == status200 -> return $ Right (strongDecode body)
        | otherwise         -> return $ Left (LBS.unpack body)
    Left e                  -> return $ Left e
  
-- | Url to the resource of which a program list can be issued.
listProgramsUrl :: String
listProgramsUrl = "/program/list"

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
serverTalk :: Request -> Manager -> IO (Status, LBS.ByteString)
serverTalk req mgr = do
  resp <- httpLbs req mgr
  return (responseStatus resp, responseBody resp)

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
