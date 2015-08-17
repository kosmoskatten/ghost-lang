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
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Network.HTTP.Client ( Manager
                           , Response (..)
                           , Request (..)
                           , RequestBody (..)
                           , httpLbs
                           , parseUrl )
import Network.HTTP.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

data LoadProgram = LoadProgram { filePath :: !FilePath }
    deriving (Generic, Show)

data LoadProgramResult = 
    LoadProgramResult { progId :: !String }
    deriving (Generic, Show)

loadProgram :: Manager -> String -> FilePath -> IO (Either String String)
loadProgram mgr baseUrl fp = do
  let msg = LoadProgram { filePath = fp }
  result <- tryString $ do
      req <- mkPostRequest (baseUrl `mappend` loadProgramUrl) msg
      serverTalk req mgr
  case result of
    Left e -> return $ Left e
    Right (stat, body)
        | stat == status201 -> do
                let answer = progId $ strongDecode body
                return $ Right answer
        | otherwise         -> return $ Left (LBS.unpack body)

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
instance FromJSON LoadProgramResult
instance ToJSON LoadProgramResult
