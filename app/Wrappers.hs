module Wrappers where

import Cli (Cli)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import Network.Socket (SockAddr)
import Servant (Handler)
import Trans (HandlerT (HandlerT, runHandlerT))
import Types (Status (Status))
import Utils (skToStr)

rootRouteWrapper :: HandlerT Status -> Handler Status
rootRouteWrapper (HandlerT val) = do
  val <- val
  case val of
    Left err -> pure err
    Right v -> pure v

fRouteWrapper :: (Connection -> Cli -> Text -> HandlerT Status) -> Connection -> Cli -> Text -> Handler Status
fRouteWrapper fn conn cli txt = do
  val <- runHandlerT $ fn conn cli txt
  case val of
    Left err -> pure err
    Right v -> pure v

cRouteWrapper :: (Connection -> Cli -> Text -> String -> HandlerT Status) -> Connection -> Cli -> SockAddr -> Text -> Handler Status
cRouteWrapper fn conn cli sk txt = do
  val <- runHandlerT $ fn conn cli txt (skToStr sk)
  case val of
    Left err -> pure err
    Right v -> pure v