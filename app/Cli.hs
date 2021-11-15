module Cli where

import Options.Applicative
  ( Parser,
    help,
    long,
    metavar,
    strOption,
    switch,
  )

data Cli = Cli
  { pastesDir :: String,
    dbPath :: String,
    repopulateDb :: Bool
  }

opts :: Parser Cli
opts =
  Cli
    <$> strOption
      ( long "paste-dir"
          <> metavar "PASTES_DIR"
          <> help "The directory where pastes will be saved"
      )
    <*> strOption
      ( long "db-path"
          <> metavar "DATABASE_PATH"
          <> help "The path to the database (sqlite .db file)"
      )
    <*> switch
      ( long "repopulateDb"
          <> help "Whether to repopulate the taken switches from existing pastes in the provided paste directory"
      )