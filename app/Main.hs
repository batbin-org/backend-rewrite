module Main where

import Control.Exception (SomeException)
import Data.Aeson (encode)
import Data.Proxy
import Data.Text (Text)
import GHC.Stack (callStack, getCallStack)
import Network.HTTP.Types.Status (status500)
import Network.Wai (Request, Response, responseLBS)
import Network.Wai.Handler.Warp
import Routes
import Servant
import Types
import Wrappers (fcRouteWrapper, rootRouteWrapper)

errHandler :: SomeException -> Response
errHandler se = do
  responseLBS status500 [] (encode $ Status False "Something went wrong!")

erSettings :: Int -> Settings
erSettings port = setOnExceptionResponse errHandler $ setPort port defaultSettings

ebSettings :: Maybe Request -> SomeException -> IO ()
ebSettings _ se = do
  putStrLn "\n---------- [ERR] Exception thrown with message:"
  print se

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

  runSettings (setOnException ebSettings $ erSettings port) app
