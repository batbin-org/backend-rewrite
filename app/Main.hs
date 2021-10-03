module Main where

import Data.Proxy
import Data.Text (Text)
import Network.Wai.Handler.Warp
import Routes
import Servant
import Trans
import Types

main :: IO ()
main = do
  let port = 8080 :: Int
  let app = serve (Proxy :: Proxy BatbinAPI) (routeWrapper root :<|> undefined :<|> undefined) --(rootWrapper :<|> fetch :<|> create)
  putStrLn $ "Starting server running on port " <> show port

  run port app
