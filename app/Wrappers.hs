module Wrappers where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
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

fRouteWrapper :: (Text -> HandlerT Status) -> Text -> Handler Status
fRouteWrapper fn txt = do
  val <- runHandlerT $ fn txt
  case val of
    Left err -> pure err
    Right v -> pure v

cRouteWrapper :: (Text -> String -> HandlerT Status) -> SockAddr -> Text -> Handler Status
cRouteWrapper fn sk txt = do
  ip <- liftIO $ skToStr sk
  val <- runHandlerT $ fn txt ip
  case val of
    Left err -> pure err
    Right v -> pure v