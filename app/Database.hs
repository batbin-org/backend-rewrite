module Database where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.SQLite.Simple (Connection (Connection), Only (Only), ToRow (toRow), execute, execute_, query, query_)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import System.Directory (doesFileExist)

data Identifier = Identifier {name :: T.Text, taken :: Bool}

instance FromRow Identifier where
  fromRow = Identifier <$> field <*> field

instance ToRow Identifier where
  toRow (Identifier name taken) = toRow (name, taken)

migrate :: Connection -> IO ()
migrate conn = do
  let sql =
        "CREATE TABLE IF NOT EXISTS identifier\
        \(id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, taken BOOLEAN,\
        \ UNIQUE(name))"
  execute_ conn sql

populateFromFile :: Connection -> String -> IO ()
populateFromFile c f = do
  doesFileExist f >>= \de ->
    if not de
      then error "File for population does not exist!"
      else do
        names <- T.lines <$> TIO.readFile f
        let mapFn txt =
              execute
                c
                "INSERT INTO identifier (name, taken) VALUES (?, ?)"
                (Identifier txt False)
        mapM_ mapFn names

markAsTaken :: Connection -> T.Text -> IO ()
markAsTaken c n = execute c "UPDATE identifier SET taken = 1 WHERE name = ?" (Only n)

getRandomName :: Connection -> IO T.Text
getRandomName c = do
  randomName <-
    query_
      c
      "SELECT * FROM table \
      \WHERE id \
      \IN (SELECT id FROM table ORDER BY RANDOM() LIMIT 1)" ::
      IO [Identifier]
  pure $ name $ head randomName