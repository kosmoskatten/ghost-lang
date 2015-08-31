{-# LANGUAGE OverloadedStrings #-}
-- | Encapsulation of fast-logger to provide a logger interface for
-- ghost products. The API is built up of both very specific logger
-- functions tailor made for specific data types, and also lower
-- performance generic logger functions.
module GhostLang.GLog
    ( GLog
    , newStdoutGLog

    -- Ghost application APIs.
    , logHttpReqResp
    , logString
    ) where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Time (getCurrentTime)
import System.Log.FastLogger ( LoggerSet
                             , LogStr
                             , defaultBufSize
                             , pushLogStrLn
                             , newStdoutLoggerSet
                             , toLogStr
                             )
import qualified Data.ByteString.Char8 as BS

-- | A thin type wrapper on top of LoggerSet.
newtype GLog = GLog LoggerSet

-- | Create a new stdout targeting logger.
newStdoutGLog :: IO GLog
newStdoutGLog = GLog <$> newStdoutLoggerSet defaultBufSize

-- | Log a line with details about the request and the response.
-- Format. <timestamp> : <http method> <resource url> - <resp code>/<resp msg>
logHttpReqResp :: GLog -> ByteString -> ByteString -> Int -> ByteString -> IO ()
logHttpReqResp glog method url respC respM =
  logOut glog $ logHttpReqRespStr method url respC respM

logHttpReqRespStr :: ByteString -> ByteString -> Int -> ByteString -> [LogStr]
logHttpReqRespStr method url respC respM = 
    [ toLogStr method
    , toLogStr (" " :: String)
    , toLogStr url
    , toLogStr (" - " :: String)
    , toLogStr (show respC)
    , toLogStr ("/" :: String)
    , toLogStr respM
    ]

-- | A generic log function for simple strings.
logString :: GLog -> String -> IO ()
logString glog str = logOut glog [toLogStr str]

logOut :: GLog -> [LogStr] -> IO ()
logOut (GLog logger) xs = do
  now <- toLogStr <$> getCurrentTimeBS
  pushLogStrLn logger $ now <> toLogStr (" : " :: String) <> mconcat xs

getCurrentTimeBS :: IO ByteString
getCurrentTimeBS = BS.pack . show <$> getCurrentTime

