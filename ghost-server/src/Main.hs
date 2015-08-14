{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString)
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import System.Random (randomRIO)
import Text.Printf (printf)

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  chunk <- genPayload
  run 8080 $ router chunk

router :: ByteString -> Application
router chunk request respond = respond $ 
    case rawPathInfo request of
      "/download"
          | requestMethod request == "GET" -> download chunk request
          | otherwise                      -> methodNotAllowed
      
      _ -> notFound

download :: ByteString -> Request -> Response
download chunk request =
    case requestedSize request of
      Just amount ->
          responseStream status200 
                         [("Content-Type", "text/plain")] $
                         generator amount chunk
      Nothing     ->
          responseLBS status400
                      [("Content-Type", "text/plain")]
                      "Missing or malformed size parameter"
    where
      requestedSize :: Request -> Maybe Int
      requestedSize req = do
        val  <- lookup "size" $ queryString req
        val' <- BS.unpack <$> val -- Also evaluating second level of Maybe
        maybeInt val'

methodNotAllowed :: Response
methodNotAllowed = 
    responseLBS status405 
                [("Allow", "GET"), ("Content-Type", "text/plain")] 
                "Method not allowed"

notFound :: Response
notFound = responseLBS status404 
                       [("Content-Type", "text/plain")] 
                       "Resource not found"

generator :: Int -> ByteString -> StreamingBody
generator amount chunk write flush = go amount
    where
      go :: Int -> IO ()
      go n
          | n == 0         = return ()
          | n >= chunkSize = do
             write $ byteString chunk
             flush
             go (n - chunkSize)
          | otherwise      = do
             write $ byteString $ BS.take n chunk
             flush
             return ()

genPayload :: IO ByteString
genPayload = BS.pack <$> (replicateM chunkSize $ randomRIO (' ', '~'))

chunkSize :: Int
chunkSize = 32768

maybeInt :: String -> Maybe Int
maybeInt str =
    case reads str of
      [(value, "")] -> Just value
      _             -> Nothing

