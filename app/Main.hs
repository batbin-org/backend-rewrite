module Main where

import Control.Exception (SomeException)
import Data.Proxy
import Data.Text (Text)
import Network.Wai (Response, responseLBS)
import Network.Wai.Handler.Warp
import Routes
import Servant
import Trans
import Types
import Wrappers (fcRouteWrapper, rootRouteWrapper)

main :: IO ()
main = do
  let port = 8080 :: Int
  let app =
        serve
          (Proxy :: Proxy BatbinAPI)
          ( rootRouteWrapper root
              :<|> fcRouteWrapper fetch
              :<|> fcRouteWrapper create
          )
  putStrLn $ "Starting server running on port " <> show port

  run port app
