module Main where

import Control.Exception (SomeException)
import Data.Aeson (encode)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Network.HTTP.Types.Status (status500)
import Network.Wai (Request, Response, responseLBS)
import Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    runSettings,
    setOnException,
    setOnExceptionResponse,
    setPort,
  )
import Routes (create, fetch, root)
import Servant (Proxy (..), serve, type (:<|>) ((:<|>)))
import Types (BatbinAPI, Status (Status))
import Wrappers (cRouteWrapper, fRouteWrapper, rootRouteWrapper)

-- wai-wide exception handler
erSettings :: Int -> Settings
erSettings port = setOnExceptionResponse errHandler $ setPort port defaultSettings

errHandler :: SomeException -> Response
errHandler se = do
  responseLBS status500 [] (encode $ Status False "Something went wrong!")

-- wai-wide exception logger
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
              :<|> fRouteWrapper fetch
              :<|> cRouteWrapper create
          )
  putStrLn $ "[i] starting paste server on port " <> show port

  runSettings (setOnException ebSettings $ erSettings port) app
