module Main where

import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.Proxy
import Data.Text (Text)
import Network.HTTP.Types.Status (status500)
import Network.Wai (Response, responseLBS)
import Network.Wai.Handler.Warp
import Routes
import Servant
import Types
import Wrappers (fcRouteWrapper, rootRouteWrapper)

errHandler :: SomeException -> Response
errHandler se = do
  responseLBS status500 [] (encode $ Status False "Something went wrong!")

ecSettings :: Int -> Settings
ecSettings port = setOnExceptionResponse errHandler $ setPort port defaultSettings

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

  runSettings (ecSettings port) app
