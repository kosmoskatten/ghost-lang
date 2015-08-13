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

app :: Application
app _ respond = do
  putStrLn "Hepp"
  respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello web!"

download :: ByteString -> Request -> Response
download chunk _ = 
    responseStream status200 
                   [("Content-Type", "text/plain")] $
                   generator 1000 chunk

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
chunkSize = 256


