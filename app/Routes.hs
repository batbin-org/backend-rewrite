module Routes where

import Cli (Cli (pastesDir))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Char8 (readInt)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.UTF8 as B (fromString)
import Data.Text as T (Text, length, unpack)
import Data.Text.IO as TIO (readFile, writeFile)
import Database (getRandomName, markAsTaken)
import Database.Redis (runRedis)
import qualified Database.Redis as R
import Database.SQLite.Simple (Connection)
import System.Directory (doesFileExist)
import Text.Regex (matchRegex)
import Trans (HandlerT, liftHT, succeed)
import Types (Status (Status))
import Utils
  ( ErrorTransform (Replace, Suppress),
    Failable ((<?>)),
    alphabets,
    (<!?>),
    (<?!>),
  )

pastesPerHour :: Int
pastesPerHour = 100

root :: HandlerT Status
root = succeed "BatBin Backend Server (Rewritten)"

fetch :: Connection -> Cli -> Text -> HandlerT Text
fetch conn cli id = do
  let path = pastesDir cli <> "/" <> unpack id

  valid <-
    matchRegex alphabets (unpack id)
      <?> Replace "Paste ID cannot contain non-alphabet characters!"

  liftIO (doesFileExist path)
    >>= (<?!>) (Replace "The provided paste ID does not exist!")

  liftIO $ TIO.readFile path

create :: Connection -> R.Connection -> Cli -> Text -> String -> HandlerT Status
create conn rconn cli content ip = do
  rn <- liftIO $ getRandomName conn
  let path = pastesDir cli <> "/" <> unpack rn
  let bip = B.fromString ip

  liftIO (not <$> doesFileExist path) >>= (<?!>) Suppress

  (T.length content < 50000) <!?> Replace "Paste too large!"
  (T.length content /= 0) <!?> Replace "Paste cannot be empty!"

  liftIO $ TIO.writeFile path content
  liftIO $ markAsTaken conn rn

  numberOfPastes <-
    liftIO (runRedis rconn $ R.get bip)
      >>= flip (<?>) Suppress

  case numberOfPastes of
    Nothing -> liftIO $ runRedis rconn $ R.setex bip 3600 (B.fromString "1") >> pure ()
    Just n -> do
      val <- readInt n <?> Suppress
      if fst val >= 100
        then do
          ttl <- liftIO (runRedis rconn $ R.ttl bip) >>= flip (<?>) Suppress
          fail $ "Limit exceeded! Next paste can be stored in " <> show ttl <> "seconds"
          pure ()
        else do
          eIncr <- liftIO (runRedis rconn $ R.incr bip) >>= flip (<?>) Suppress
          pure ()

  succeed rn
