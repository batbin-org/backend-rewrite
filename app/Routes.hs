module Routes where

import Data.Text (Text)
import Trans
import Types

root :: HandlerT Status
root = do
  pure $ Status True "BatBin Backend Server"

fetch :: Text -> HandlerT Status
fetch id = do
  pure $ Status True "some dummy content"

create :: Text -> HandlerT Status
create content = do
  pure $ Status True "operation done"
