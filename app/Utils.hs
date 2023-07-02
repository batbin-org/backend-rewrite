{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils where

import Control.Monad.Cont (MonadIO (liftIO))
import Data.IP
  ( IP (IPv4, IPv6),
    fromHostAddress,
    fromHostAddress6,
  )
import Data.Text (Text, unpack)
import Network.Socket (SockAddr (..))
import Trans (HandlerT (HandlerT))
import Types (Status (Status))

-- Stringable
class Stringable a where
  stringify :: a -> String

instance {-# OVERLAPS #-} Stringable String where
  stringify = id

instance {-# OVERLAPS #-} Stringable Char where
  stringify x = [x]

instance {-# OVERLAPS #-} Stringable Text where
  stringify = unpack

instance Show a => Stringable a where
  stringify = show

-- Error handling helpers
data ErrorTransform = Suppress String | Reflect | Replace Text

-- usage example of <?>
-- demo :: HandlerT Status
-- demo = do
--   -- Something went wrong!
--   somethingThatFails <?> Suppress
--   -- Failed to query the database
--   somethingThatFails <?> Reflect
--   -- A custom message
--   somethingThatFails <?> Replace "A custom message"
--   succeed "This is how error handling works!"

class Monad m => Failable m where
  (<?>) :: m a -> ErrorTransform -> HandlerT a

instance Failable Maybe where
  x <?> t = case x of
    Nothing -> case t of
      Suppress log -> do
        liftIO $ print log
        fail "Something went wrong!"
      Replace err -> fail (unpack err)
      -- we've got nothing to reflect
      -- so we might as well suppress
      Reflect -> fail "Something went wrong!"
    Just v -> pure v

instance Stringable l => Failable (Either l) where
  x <?> t = case x of
    Left err -> do
      liftIO $ putStrLn ("ERR> " <> stringify err)
      case t of
        Suppress log -> do
          liftIO $ print log
          fail "Something went wrong!"
        Replace err' -> fail (unpack err')
        Reflect -> fail (stringify err)
    Right v -> pure v

(<?!>) :: ErrorTransform -> Bool -> HandlerT Bool
(<?!>) t v =
  if v
    then pure v
    else case t of
      Suppress log -> do
        liftIO $ print log
        fail "Something went wrong!"
      Reflect -> fail "A bool was false!"
      Replace t -> fail (unpack t)

(<!?>) :: Bool -> ErrorTransform -> HandlerT Bool
(<!?>) = flip (<?!>)

-- Generic Utilities
alphabets :: Text
alphabets = "^[a-zA-Z0-9]+$"

skToStr :: SockAddr -> String
skToStr sockAddr = case sockAddr of
  SockAddrInet _ h4 -> show $ IPv4 $ fromHostAddress h4
  SockAddrInet6 _ _ h6 _ -> show $ IPv6 $ fromHostAddress6 h6
  SockAddrUnix _ -> error "UNIX address found on the web!"
