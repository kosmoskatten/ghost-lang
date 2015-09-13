module Documentation
    ( commandDocs
    , apiDocs
    ) where

import Text.Printf (printf)

commandDocs :: [String]
commandDocs =
  [ "help"
  , " - This help screen.", ""

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
apiDocs = undefined

