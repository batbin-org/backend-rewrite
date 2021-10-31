module Main where

import Control.Exception (SomeException)
import Data.Aeson (encode)
import Data.Proxy (Proxy (..))
import Data.Text as T (Text, unpack)
import Database (getRandomName, migrate, populateFromFile)
import Database.SQLite.Simple (open)
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
import System.Directory (doesFileExist)
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

  let db = "batbin.db"

  didExist <- doesFileExist db
  conn <- open db
  migrate conn
  if not didExist then populateFromFile conn "words_alpha.txt" else pure ()

  let app =
        serve
          (Proxy :: Proxy BatbinAPI)
          ( rootRouteWrapper root
              :<|> fRouteWrapper fetch conn
              :<|> cRouteWrapper create conn
          )

  putStrLn $ "[i] starting paste server on port " <> show port
  n <- getRandomName conn
  putStrLn $ T.unpack n
  runSettings (setOnException ebSettings $ erSettings port) app
