module Cli where

import Options.Applicative

data Cli = Cli
  { pastesDir :: String,
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
    <*> switch
      ( long "repopulateDb"
          <> help "Whether to repopulate the taken switches from existing pastes in the provided paste directory"
      )