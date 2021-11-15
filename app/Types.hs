module Types where

import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString.Lazy.Char8 (unpack)
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
import Servant.API (RemoteHost)

data Paste = Paste
  { id :: Text,
    content :: Text
  }
  deriving (Eq, Show, Read, Generic)

data Status = Status
  { success :: Bool,
    message :: Text
  }
  deriving (Eq, Read, Generic)

instance Show Status where
  show = unpack . encode

instance FromJSON Paste

instance ToJSON Paste

instance FromJSON Status

instance ToJSON Status

type BatbinAPI =
  -- GET / -> Text
  Get '[JSON] Status
    -- GET /paste/:id -> Paste Content (in PlainText)
    :<|> "api" :> "v2" :> "paste" :> Capture "id" Text :> Get '[PlainText] Text
    -- POST /paste -> Status (in JSON)
    :<|> "api" :> "v2" :> "paste" :> RemoteHost :> ReqBody '[PlainText] Text :> Post '[JSON] Status
