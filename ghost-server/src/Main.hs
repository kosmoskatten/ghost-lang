{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Text.Printf (printf)

main :: IO ()
main = run 8080 router

router :: Application
router request respond = respond $ 
    case rawPathInfo request of
      "/download"
          | requestMethod request == "GET" -> download request
          | otherwise                      -> methodNotAllowed
      
      _ -> notFound

app :: Application
app _ respond = do
  putStrLn "Hepp"
  respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello web!"

download :: Request -> Response
download _ = responseLBS status200 [("Content-Type", "text/plain")] "Hepp"

methodNotAllowed :: Response
methodNotAllowed = 
    responseLBS status405 
                [("Allow", "GET"), ("Content-Type", "text/plain")] 
                "Method not allowed"

notFound :: Response
notFound = responseLBS status404 
                       [("Content-Type", "text/plain")] 
                       "Resource not found"
