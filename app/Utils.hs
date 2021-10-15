module Utils where

import Data.Text (Text, unpack)
import Trans (HandlerT (HandlerT))
import Types (Status (Status))

type RespType = Status

htFromMaybe :: Text -> Maybe a -> HandlerT a
htFromMaybe t m = case m of
  Nothing -> fail (unpack t)
  Just v -> pure v

htFromEither :: Text -> Either e a -> HandlerT a
htFromEither t e = case e of
  Left err -> fail (unpack t)
  Right v -> pure v
