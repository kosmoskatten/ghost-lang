module Main
    ( main
    ) where

import GhostLang.Node (runNode)
import System.Environment (getArgs)

main :: IO ()
main = do
  [listenPort] <- getArgs
  runNode $ read listenPort
