module Routes where

import Control.Monad.IO.Class
import Data.Text (Text, unpack)
import Trans (HandlerT, liftHT, succeed)
import Types (Status (Status))
import Utils

somethingThatFails :: Either String Int
somethingThatFails = Left "Failed to query the database"

root :: HandlerT Status
root = do
  -- Something went wrong!
  somethingThatFails <!> True
  -- Failed to query the database
  somethingThatFails <!> False
  -- A custom message
  somethingThatFails <?> "A custom message"
  succeed "BatBin Backend Server"

fetch :: Text -> HandlerT Status
fetch id = do
  pure $ Status True "some dummy content"

create :: Text -> HandlerT Status
create content = do
  pure $ Status True "operation done"
