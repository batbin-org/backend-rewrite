module Main where

import           Cli (Cli (Cli, dbPath, pastesDir, port, repopulateDb), opts)
import           Control.Exception (SomeException)
import           Control.Monad (when, unless)
import           Data.Aeson (encode)
import           Data.Proxy (Proxy (..))
import           Data.Text as T (Text, unpack)
import qualified Data.Text.IO as TIO (putStrLn)
import           Database (getRandomName, migrate, populateFromFile, repopulateFromFs)
import           Database.Redis (checkedConnect, defaultConnectInfo)
import           Database.SQLite.Simple (open)
import           Network.HTTP.Types.Status (status500)
import           Network.Wai (Request, Response, responseLBS)
import           Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    runSettings,
    setOnException,
    setOnExceptionResponse,
    setPort,
  )
import           Options.Applicative
  ( execParser,
    fullDesc,
    header,
    info,
    progDesc,
  )
import           Routes (create, fetch, root)
import           Servant (Proxy (..), serve, type (:<|>) ((:<|>)))
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import           TextShow (TextShow (showt))
import           Types (BatbinAPI, Status (Status))
import           Wrappers (cRouteWrapper, fRouteWrapper, rootRouteWrapper)

-- wai-wide exception handler
erSettings :: Int -> Settings
erSettings port = setOnExceptionResponse errHandler $ setPort port defaultSettings

errHandler :: SomeException -> Response
errHandler se = do
  responseLBS status500 [] (encode $ Status False "Something went wrong!")

-- wai-wide exception logger
ebSettings :: Maybe Request -> SomeException -> IO ()
ebSettings _ se = do
  let str = showt se
  case str of
    "Thread killed by timeout manager" -> pure ()
    _ -> do
      putStrLn "\n---------- [ERR] Exception thrown with message:"
      TIO.putStrLn str

batbinServer :: Cli -> IO ()
batbinServer cli = do
  let db = dbPath cli

  didExist <- doesFileExist db
  pdExist <- createDirectoryIfMissing True (pastesDir cli)

  putStrLn "accessing the database..."
  conn <- open db

  putStrLn "connecting to the Redis server..."
  rconn <- checkedConnect defaultConnectInfo

  putStrLn "running migration..."
  migrate conn

  unless didExist $ do
    populateFromFile conn "words_alpha.txt"
    putStrLn "\rPopulation complete!"

  when (repopulateDb cli) $ do
    repopulateFromFs conn (pastesDir cli)

  let app =
        serve
          (Proxy :: Proxy BatbinAPI)
          ( rootRouteWrapper root
              :<|> fRouteWrapper fetch conn cli
              :<|> cRouteWrapper create conn rconn cli
          )

  TIO.putStrLn $ "[i] starting paste server on port " <> showt (port cli)
  runSettings (setOnException ebSettings $ erSettings (port cli)) app

main :: IO ()
main = batbinServer =<< execParser opts'
  where
    opts' =
      info
        opts
        ( fullDesc
            <> progDesc "Run a paste server or use as a client to interact with a paste server"
            <> header "Batbin Server+Client"
        )
