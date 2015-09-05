module GhostLang.Interpreter.WebClient
    ( httpGet
    ) where

import Control.Monad.Trans.Resource (runResourceT)
import GHC.Int (Int64)
import GhostLang.Conduit (($=+), ($$+-), byteCounter, shape)
import GhostLang.RuntimeState (HttpStatus (..))
import Network.HTTP.Conduit
import Network.HTTP.Types

type RetVal = (HttpStatus, Int64)

-- | Fetch a web resource.
httpGet :: Manager -> String -> Maybe Int -> IO RetVal
httpGet mgr url maybePace = do
  req <- parseUrl url
  runResourceT $ do
    resp <- http req mgr
    bytes <- 
        case maybePace of
          Just pace -> responseBody resp $=+ shape pace $$+- byteCounter
          Nothing   -> responseBody resp $$+- byteCounter
    return (fromStatus $ responseStatus resp, fromIntegral bytes)
  
fromStatus :: Status -> HttpStatus
fromStatus status
    | status == status200 = Success
    | otherwise           = Failure
