module Routes where

import Cli (Cli)
import Control.Monad.IO.Class
import Data.Text (Text, unpack)
import Database (getRandomName)
import Database.SQLite.Simple (Connection)
import Trans (HandlerT, liftHT, succeed)
import Types (Status (Status))
import Utils

somethingThatFails :: Either String Int
somethingThatFails = Left "Failed to query the database"

demo :: HandlerT Status
demo = do
  -- Something went wrong!
  somethingThatFails <?> Suppress
  -- Failed to query the database
  somethingThatFails <?> Reflect
  -- A custom message
  somethingThatFails <?> Replace "A custom message"
  succeed "This is how error handling works!"

root :: HandlerT Status
root = do
  succeed "BatBin Backend Server"

fetch :: Connection -> Cli -> Text -> HandlerT Status
fetch conn cli id = do
  rn <- liftIO $ getRandomName conn
  succeed rn

create :: Connection -> Cli -> Text -> String -> HandlerT Status
create conn cli content ip = do
  rn <- liftIO $ getRandomName conn
  succeed ""
