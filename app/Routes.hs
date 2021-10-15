module Routes where

import Control.Monad.IO.Class
import Data.Text (Text)
import Trans (HandlerT, liftHT)
import Types (Status (Status))
import Utils

somethingThatFails :: Either String Int
somethingThatFails = Left "Failed to query the database"

nomethingThatFails :: Either Status Int
nomethingThatFails = Left $ Status False "Failed to query the database"

root :: HandlerT Status
root = do
  liftHT nomethingThatFails
  htFromEither "yea" somethingThatFails
  liftIO (pure somethingThatFails) >>= htFromEither "yes"
  pure $ Status True "BatBin Backend Server"

fetch :: Text -> HandlerT Status
fetch id = do
  pure $ Status True "some dummy content"

create :: Text -> HandlerT Status
create content = do
  pure $ Status True "operation done"
