module Routes where

import Cli (Cli (pastesDir))
import Control.Monad.IO.Class
import Data.Text as T (Text, length, unpack)
import Data.Text.IO as TIO
import Database (getRandomName)
import Database.SQLite.Simple (Connection)
import System.Directory (doesFileExist)
import Text.Regex (matchRegex)
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
root = succeed "BatBin Backend Server (Rewritten)"

fetch :: Connection -> Cli -> Text -> HandlerT Status
fetch conn cli id = do
  let path = pastesDir cli <> "/" <> unpack id

  valid <-
    matchRegex alphabets (unpack id)
      <?> Replace "Paste ID cannot contain non-alphabet characters!"

  liftIO (doesFileExist path)
    >>= (<?!>) (Replace "The provided paste ID does not exist!")

  content <- liftIO $ TIO.readFile path

  succeed content

create :: Connection -> Cli -> Text -> String -> HandlerT Status
create conn cli content ip = do
  rn <- liftIO $ getRandomName conn
  let path = pastesDir cli <> "/" <> unpack rn

  liftIO (doesFileExist path) >>= (<?!>) Suppress

  (T.length content > 50000) <!?> Replace "Paste too large!"
  (T.length content == 0) <!?> Replace "Paste cannot be empty!"

  liftIO $ TIO.writeFile path content

  succeed "Operation successful!"
