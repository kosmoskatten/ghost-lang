{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8080 app

app :: Application
app _ respond = do
  putStrLn "Hepp"
  respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello web!"
