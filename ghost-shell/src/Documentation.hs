{-# LANGUAGE OverloadedStrings #-}
module Documentation
    ( commandDocs
    , apiDocs
    ) where

import Data.Aeson.Encode.Pretty (encodePretty)
import GhostLang.API
import qualified Data.ByteString.Lazy.Char8 as LBS

commandDocs :: [String]
commandDocs =
  [ "help"
  , " - This help screen.", ""

  , "help-api"
  , " - List API documentation.", ""

  , "load-program <filepath>"       
  , " - Load the program at filepath.", ""

  , "list-programs"                 
  , " - List all registered programs.", ""

  , "list-selected-program"
  , " - List the patterns for the selected program.", ""

  , "list-patterns"               
  , " - List all in-flight patterns.", ""

  , "get-http-config"
  , " - List the node's http config.", ""

  , "set-http-config <server> <port>" 
  , " - Set the node's http config.", ""

  , "run-named-pattern <name> trace={true|false} [src=<ip address>]"
  , " - Run a named pattern from the selected/registered program."
  , " - The trace option specify if the pattern execution is traced,"
  , " - and the src option is specifying src ip address (if any).", ""

  , "run-random-pattern trace={true|false} [src=<ip address>]"
  , " - Run a random pattern from the selected/registered program."
  , " - The trace option specify if the pattern execution is traced,"
  , " - and the src option is specifying src ip address (if any).", ""

  , "list-global-counter"
  , " - List the global counter.", ""

  , "list-selected-counter <resource url>"
  , " - List the counter for the selected pattern resource url.", ""

  , "list-selected-status <resource url>"
  , " - List the status for the selected pattern resource url.", ""

  , "quit"
  , " - Quit the shell (you can also quit by ^C or ^D)."
  ]

apiDocs :: [String]
apiDocs =
  [ "-= Load a program on the node =-", ""
  , "HTTP POST on /program/load with JSON (example) payload:"
  , encodePretty'Str ProgramPath { programPath = "/example/Main.gl" }, ""
  , "If program loading went out ok, response code 201 with (example) payload:"
  , encodePretty'Str Resource { resourceUrl = "/program/XYZ" }, ""
  , "If program loading failed, response code 409 is returned.", ""

  , "-= List all registered programs on the node =-", ""
  , "HTTP GET on /program/list"
  , "Response code 200 with (example) payload:"
  , encodePretty'Str [ Resource { resourceUrl = "/program/XYZ" }
                     , Resource { resourceUrl = "/program/ABC" } ], ""

  , "-= List the patterns for the selected program =-", ""
  , "HTTP GET on (example) /program/XYZ/list"
  , "Response code 200 with (example) payload:"
  , encodePretty'Str [ PatternInfo { patternName = "pattern1"
                                   , patternWeight = 1 }
                     , PatternInfo { patternName = "pattern2"
                                   , patternWeight = 2 } ], ""

  , "-= List all in-flight patterns on the node =-"
  , "HTTP GET on /pattern/list"
  , "Response code 200 with (example) payload:"
  , encodePretty'Str [ Resource { resourceUrl = "/pattern/XYZ" }
                     , Resource { resourceUrl = "/pattern/ABC" } ], ""

  , "-= List the node's http service config =-"
  , "HTTP GET on /configuration/http"
  , "Response code 200 with (example) payload:"
  , encodePretty'Str $ Service { serviceAddress = "http://server-host"
                               , servicePort    = 8080 }, ""

  , "-= Set the node's http service config =-"
  , "HTTP PUT on /configuration/http with (example) payload:"
  , encodePretty'Str $ Service { serviceAddress = "http://server-host"
                               , servicePort    = 8080 }
  , "Response code 200", ""

  , "-= Run a named pattern from the selected program =-"
  , "HTTP POST on (example) /program/XYZ/named-pattern with (example) payload:"
  , encodePretty'Str $ 
      NamedPattern { execPattern = "pattern1"
                   , execParams  = ExecParams { shallTrace = True
                                              , srcIp      = Just "10.0.0.3" }
                   }, ""
  , "If anything went out ok, response code 201 with (example) payload:"
  , encodePretty'Str $ Resource { resourceUrl = "/pattern/XYZ" }
  , "If the selected program not was found response code 404 is returned.", ""

  , "-= Run a random pattern from the selected program =-"
  , "HTTP POST on (example) /program/XYZ/random-pattern with (example) payload:"
  , encodePretty'Str $ ExecParams { shallTrace = False
                                  , srcIp      = Nothing }, ""
  , "If anything went out ok, response code 201 with (example) payload:"
  , encodePretty'Str $ Resource { resourceUrl = "/pattern/XYZ" }
  , "If the selected program not was found response code 404 is returned.", ""
  ]

encodePretty'Str :: ToJSON a => a -> String
encodePretty'Str = LBS.unpack . encodePretty

