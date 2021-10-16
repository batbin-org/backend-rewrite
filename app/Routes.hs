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
  somethingThatFails <?> Suppress
  -- Failed to query the database
  somethingThatFails <?> Reflect
  -- A custom message
  somethingThatFails <?> Replace "A custom message"
  succeed "BatBin Backend Server"

fetch :: Text -> HandlerT Status
fetch id = do
  pure $ Status True "some dummy content"

create :: Text -> String -> HandlerT Status
create content ip = do
  pure $ Status True "operation done"
