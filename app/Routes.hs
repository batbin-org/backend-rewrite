module Routes where

import Data.Text (Text)
import Servant
import Types

root :: Handler Text
root = do
  pure "BatBin Backend Server"

fetch :: Text -> Handler Status
fetch id = do
  pure $ Status True "some dummy content"

create :: Text -> Handler Status
create content = do
  pure $ Status True "operation done"
