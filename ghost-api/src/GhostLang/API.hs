{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module GhostLang.API
    ( LoadProgram (..)
    , LoadProgramResult (..)
    , loadProgram
    , module Data.Aeson
    ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Maybe (fromJust, fromMaybe)
import GHC.Generics (Generic)
import Network.HTTP.Client ( Manager
                           , Response (..)
                           , Request (..)
                           , RequestBody (..)
                           , httpLbs
                           , parseUrl )
import Network.HTTP.Types
import qualified Data.ByteString.Char8 as BS

data LoadProgram = LoadProgram { filepath :: !FilePath }
    deriving (Generic, Show)

data LoadProgramResult = 
    LoadProgramResult { description :: !(Maybe String) }
    deriving (Generic, Show)

loadProgram :: Manager -> String -> FilePath -> IO (Either String ())
loadProgram mgr baseUrl filePath = do
  let msg = LoadProgram { filepath = filePath }
  result <- tryString $ do
      req <- mkPostRequest (baseUrl `mappend` loadProgramUrl) msg
      jsonTalk req mgr :: IO (Status, LoadProgramResult)
  case result of
    Left  e -> return $ Left e
    Right (stat, answer)
        | stat == status201 -> return $ Right ()
        | stat == status409 -> 
            return $ Left (fromMaybe "" (description answer))
        | otherwise        -> return $ Left (BS.unpack $ statusMessage stat)

-- | Url to the resource on which a program load can be issued.
loadProgramUrl :: String
loadProgramUrl = "/program/load"

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

-- | Send a request and expect a JSON reply. Can throw exceptions both
-- on network stuff and decoding of JSON.
jsonTalk :: FromJSON a => Request -> Manager -> IO (Status, a)
jsonTalk req mgr = do
  resp <- httpLbs req mgr
  return (responseStatus resp, fromJust $ decode (responseBody resp))

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
instance FromJSON LoadProgramResult
instance ToJSON LoadProgramResult
