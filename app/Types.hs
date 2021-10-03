module Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
  ( Capture,
    Get,
    JSON,
    PlainText,
    Post,
    ReqBody,
    type (:<|>),
    type (:>),
  )

data Paste = Paste
  { id :: Text,
    content :: Text
  }
  deriving (Eq, Show, Read, Generic)

data Status = Status
  { success :: Bool,
    message :: Text
  }
  deriving (Eq, Show, Read, Generic)

instance FromJSON Paste

instance ToJSON Paste

instance FromJSON Status

instance ToJSON Status

type BatbinAPI =
  -- GET / -> Text
  Get '[JSON] Status
    -- GET /paste/:id -> Paste Content (in PlainText)
    :<|> "paste" :> Capture "id" Text :> Get '[JSON] Status
    -- POST /paste/:pasteContent -> Status (in JSON)
    :<|> "paste" :> ReqBody '[JSON] Text :> Post '[JSON] Status
