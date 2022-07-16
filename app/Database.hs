module Database where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.SQLite.Simple (Connection (Connection), Only (Only), ToRow (toRow), execute, execute_, query, query_)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import Text.PortableLines as TPL (lines)

data Identifier = Identifier {id :: Int, name :: T.Text, taken :: Bool}

instance FromRow Identifier where
  fromRow = Identifier <$> field <*> field <*> field

instance ToRow Identifier where
  toRow (Identifier _ name taken) = toRow (name, taken)

migrate :: Connection -> IO ()
migrate c = do
  let sql =
        "CREATE TABLE IF NOT EXISTS identifier\
        \(id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, taken BOOLEAN,\
        \ UNIQUE(name))"
  execute_ c sql

populateFromFile :: Connection -> String -> IO ()
populateFromFile c f = do
  putStrLn $ "Beginning database population from file '" <> f <> "'"
  doesFileExist f >>= \de ->
    if not de
      then error "File for population does not exist!"
      else do
        names <- TPL.lines <$> readFile f
        let lines = length names
        let mapFn tup = do
              execute
                c
                "INSERT INTO identifier (name, taken) VALUES (?, ?)"
                (Identifier (-1) (snd tup) False)
              putStr $ "\r" <> show (fst tup) <> " / " <> show lines
        mapM_ mapFn (zip [0 ..] (map T.pack names))

markAsTaken :: Connection -> T.Text -> IO ()
markAsTaken c n = execute c "UPDATE identifier SET taken = 1 WHERE name = ?" (Only n)

getRandomName :: Connection -> IO T.Text
getRandomName c = do
  randomName <-
    query_
      c
      "SELECT * FROM identifier \
      \WHERE id \
      \IN (SELECT id FROM identifier WHERE taken = 0 ORDER BY RANDOM() LIMIT 1)" ::
      IO [Identifier]
  pure $ name $ head randomName

repopulateFromFs :: Connection -> String -> IO ()
repopulateFromFs c pd = do
  doesDirectoryExist pd >>= \b ->
    if not b
      then error "directory to repopulate from does not exist!"
      else do
        files <- listDirectory pd
        mapM_ (markAsTaken c . T.pack) files