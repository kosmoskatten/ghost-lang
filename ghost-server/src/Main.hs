module Main
    ( main
    ) where

import GhostLang.Server (runServer)

main :: IO ()
main = runServer 8080
