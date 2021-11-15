module Wrappers where

import Cli (Cli)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson.Text (encodeToLazyText)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import qualified Database.Redis as R (Connection)
import Database.SQLite.Simple (Connection)
import Network.Socket (SockAddr)
import Servant (Handler)
import Trans (HandlerT (HandlerT, runHandlerT))
import Types (Status (Status, message))
import Utils (skToStr)

rootRouteWrapper :: HandlerT Status -> Handler Status
rootRouteWrapper (HandlerT val) = do
  val <- val
  case val of
    Left err -> pure err
    Right v -> pure v

fRouteWrapper :: (Connection -> Cli -> Text -> HandlerT Text) -> Connection -> Cli -> Text -> Handler Text
fRouteWrapper fn conn cli txt = do
  val <- runHandlerT $ fn conn cli txt
  case val of
    Left err -> pure $ "[Batbin Error] " <> message err
    Right v -> pure v

cRouteWrapper :: (Connection -> R.Connection -> Cli -> Text -> String -> HandlerT Status) -> Connection -> R.Connection -> Cli -> SockAddr -> Text -> Handler Status
cRouteWrapper fn conn rconn cli sk txt = do
  val <- runHandlerT $ fn conn rconn cli txt (skToStr sk)
  case val of
    Left err -> pure err
    Right v -> pure v