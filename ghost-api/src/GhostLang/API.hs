{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module GhostLang.API
    ( LoadProgram (..)
    , PatternInfo (..)
    , Resource (..)
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
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T

data LoadProgram = LoadProgram { filePath :: !Text }
    deriving (Generic, Show)

data PatternInfo = PatternInfo { patternName   :: !Text
                               , patternWeight :: !Int64
                               }
    deriving (Generic, Show)

data Resource = Resource { resourceId :: !Text }
    deriving (Generic, Show)

-- | Load a program on the remote node.
loadProgram :: Manager -> String -> Text -> IO (Either String Text)
loadProgram mgr baseUrl fp = do
  let msg = LoadProgram { filePath = fp }
  result <- tryString $ do
      req <- mkPostRequest (baseUrl `mappend` loadProgramUrl) msg
      serverTalk req mgr
  case result of
    Right (stat, body)
        | stat == status201 -> do
                let answer = resourceId $ strongDecode body
                return $ Right answer
        | otherwise         -> return $ Left (LBS.unpack body)
    Left e                  -> return $ Left e

-- | Url to the resource on which a program load can be issued.
loadProgramUrl :: String
loadProgramUrl = "/program/load"

-- | List the patterns for the selected program on the remote node.
listSelectedProgram :: Manager -> String -> Text 
                    -> IO (Either String [PatternInfo])
listSelectedProgram mgr baseUrl resId = do
  let url = baseUrl `mappend` (T.unpack resId) `mappend` "/list"
  result <- tryString $ do
      req <- mkGetRequest url
      serverTalk req mgr
  case result of
    Right (stat, body)
        | stat == status200 -> return $ Right (strongDecode body)
        | otherwise         -> return $ Left (LBS.unpack body)
    Left e                  -> return $ Left e

-- | List the resource urls for all loaded programs on the node.
listPrograms :: Manager -> String -> IO (Either String [Resource])
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

-- | Make a POST request carrying a JSON object. Can throw exception
-- if the url is malformed.
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

-- | Make a GET request. Can throw exception if the url is malformed.
mkGetRequest :: String -> IO Request
mkGetRequest url = do
  initReq <- parseUrl url
  let req = initReq
            { requestHeaders = [ ("Accept", "application/json") ]
            , checkStatus    = \_ _ _ -> Nothing
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

-- | Check if the response headers having application/json as it's
-- content type.
isContentJSON :: ResponseHeaders -> Bool
isContentJSON xs = 
    maybe False (== "application/json") $ lookup "Content-Type" xs

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
instance FromJSON LoadProgram
instance ToJSON LoadProgram
instance FromJSON PatternInfo
instance ToJSON PatternInfo
instance FromJSON Resource
instance ToJSON Resource
