module Main where

import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy
import Data.Text (Text)
import Network.HTTP.Types.Status (status500)
import Network.Wai (Response, responseLBS)
import Network.Wai.Handler.Warp
import Routes
import Servant
import Trans
import Types
import Wrappers (fcRouteWrapper, rootRouteWrapper)

myHandler :: SomeException -> Response
myHandler se = do
  -- liftIO $ putStrLn ""
  responseLBS status500 [] "HERE WE ARE"

mySettings :: Settings
mySettings = setOnExceptionResponse myHandler $ setPort 3002 defaultSettings

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
