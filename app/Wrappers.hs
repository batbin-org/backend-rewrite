module Wrappers where

import Data.Text (Text)
import Servant (Handler)
import Trans (HandlerT (HandlerT, runHandlerT))
import Types (Status (Status))

rootRouteWrapper :: HandlerT Status -> Handler Status
rootRouteWrapper (HandlerT val) = do
  val <- val
  case val of
    Left err -> pure err
    Right v -> pure v

fcRouteWrapper :: (Text -> HandlerT Status) -> Text -> Handler Status
fcRouteWrapper fn txt = do
  val <- runHandlerT $ fn txt
  case val of
    Left err -> pure err
    Right v -> pure v