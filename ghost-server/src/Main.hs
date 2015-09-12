module Main
    ( main
    ) where

import GhostLang.Server (runServer)
import System.Environment (getArgs)

main :: IO ()
main = do
  [listenPort] <- getArgs
  runServer $ read listenPort
