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
  [ "-= GENERAL =-"
  , "Below is the REST API documentation for ghost. Each procedure is"
  , "divided in a request and a response part describing HTTP header and"
  , "payload. URL:s may contain example resources as part of the URL,"
  , "typically those example resources are named such as ABC or XYZ."
  , "Values in JSON fields are also example values.", ""

  , "-= LOAD A PROGRAM ON THE NODE =-"
  , "Request:"
  , "POST /program/load HTTP/1.0"
  , "Content-Type: application/json"
  , encodePretty'Str ProgramPath { programPath = "/example/Main.gl" }, ""
  , "Response:"
  , "HTTP/1.1 201 Created"
  , "Content-Type: application/json"
  , encodePretty'Str Resource { resourceUrl = "/program/XYZ" }, ""
  , "Alternative Responses:"
  , "HTTP/1.1 409 Conflict", ""
  
  , "-= LIST ALL REGISTERED PROGRAMS ON THE NODE =-"
  , "Request:"
  , "GET /program/list HTTP/1.0", ""
  , "Response:"
  , "HTTP/1.1 200 OK"
  , "Content-Type: application/json"
  , encodePretty'Str [ Resource { resourceUrl = "/program/XYZ" }
                     , Resource { resourceUrl = "/program/ABC" } ], ""    

  , "-= LIST THE PATTERNS FOR THE SELECTED PROGRAM =-"
  , "Request:"
  , "GET /program/XYZ/list HTTP/1.0", ""
  , "Response:"
  , "HTTP/1.1 200 OK"
  , "Content-Type: application/json"
  , encodePretty'Str [ PatternInfo { patternName = "pattern1"
                                   , patternWeight = 1 }
                     , PatternInfo { patternName = "pattern2"
                                   , patternWeight = 2 } ], ""

  , "-= LIST ALL IN-FLIGHT PATTERNS ON THE NODE =-"
  , "Request:"
  , "GET /pattern/list HTTP/1.0", ""
  , "Response:"
  , "HTTP/1.1 200 OK"
  , "Content-Type: application/json"
  , encodePretty'Str [ Resource { resourceUrl = "/pattern/XYZ" }
                     , Resource { resourceUrl = "/pattern/ABC" } ], ""

  , "-= LIST THE NODE'S HTTP SERVICE CONFIG =-"
  , "Request:"
  , "GET /configuration/http HTTP/1.0", ""
  , "Response:"
  , "HTTP/1.1 200 OK"
  , "Content-Type: application/json"
  , encodePretty'Str $ Service { serviceAddress = "http://server-host"
                               , servicePort    = 8080 }, ""

  , "-= SET THE NODE'S HTTP SERVICE CONFIG =-"
  , "Request:"
  , "PUT /configuration/http HTTP/1.0"
  , "Content-Type: application/json"
  , encodePretty'Str $ Service { serviceAddress = "http://server-host"
                               , servicePort    = 8080 }, ""
  , "Response:"
  , "HTTP/1.1 200 OK", ""

  , "-= RUN A NAMED PATTERN FROM THE SELECTED PROGRAM =-"
  , "Request:"
  , "POST /program/XYZ/named-pattern HTTP/1.0"
  , "Content-Type: application/json"
  , encodePretty'Str $ 
      NamedPattern { execPattern = "pattern1"
                   , execParams  = ExecParams { shallTrace = True
                                              , srcIp      = Just "10.0.0.3" }
                   }, ""
  , "Response:"
  , "HTTP/1.1 201 Created"
  , "Content-Type: application/json"
  , encodePretty'Str $ Resource { resourceUrl = "/pattern/XYZ" }, ""
  , "Alternative responses:"
  , "HTTP/1.1 404 Not Found", ""

  , "-= RUN A RANDOM PATTERN FROM THE SELECTED PROGRAM =-"
  , "Request:"
  , "POST /program/XYZ/random-pattern HTTP/1.0"
  , "Content-Type: application/json"
  , encodePretty'Str $ ExecParams { shallTrace = False
                                  , srcIp      = Nothing }, ""
  , "Response:"
  , "HTTP/1.1 201 Created"
  , "Content-Type: application/json"
  , encodePretty'Str $ Resource { resourceUrl = "/pattern/XYZ" }, ""
  , "Alternative responses:"
  , "HTTP/1.1 404 Not Found", ""
  ]

encodePretty'Str :: ToJSON a => a -> String
encodePretty'Str = LBS.unpack . encodePretty

