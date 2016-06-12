module Main
    ( main
    ) where

import Data.String (fromString)
import GhostLang.Server (runServer)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [hostPreference, listenPort] ->
            runServer (fromString hostPreference) (read listenPort)
        _                            ->
            putStrLn "Usage: ghost-server <host preference> <port>"
