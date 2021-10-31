module Wrappers where

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

fRouteWrapper :: (Connection -> Text -> HandlerT Status) -> Connection -> Text -> Handler Status
fRouteWrapper fn conn txt = do
  val <- runHandlerT $ fn conn txt
  case val of
    Left err -> pure err
    Right v -> pure v

cRouteWrapper :: (Connection -> Text -> String -> HandlerT Status) -> Connection -> SockAddr -> Text -> Handler Status
cRouteWrapper fn conn sk txt = do
  val <- runHandlerT $ fn conn txt (skToStr sk)
  case val of
    Left err -> pure err
    Right v -> pure v