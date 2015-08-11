module GhostLang.Interpreter.WebClient
    ( httpGet
    , mkGetUrl
    ) where

import GHC.Int (Int64)
import GhostLang.Interpreter.InterpreterM ( InterpreterM
                                          , evalPayload
                                          , get
                                          )
import GhostLang.RuntimeState ( RuntimeState (..)
                              , NetworkConfiguration (..)
                              )
import GhostLang.Types (Payload)
import Network.HTTP.Client
import Text.Printf (printf)

httpGet :: Int64 -> InterpreterM ()
httpGet = undefined

mkGetUrl :: Payload -> InterpreterM String
mkGetUrl payload = do
  size <- evalPayload payload
  nc   <- networkConfiguration <$> get
  return $! printf "%s:%d/download?size=%ld" (httpServiceAddress nc)
                                             (httpServicePort nc) size
