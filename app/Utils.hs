{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils where

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
data ErrorTransform = Suppress | Reflect | Replace Text

class Monad m => Failable m where
  (<?>) :: m a -> ErrorTransform -> HandlerT a

instance Failable Maybe where
  x <?> t = case x of
    Nothing -> case t of
      Suppress -> fail "Something went wrong!"
      Replace err -> fail (unpack err)
      -- we've got nothing to reflect
      -- so we might as well suppress
      Reflect -> fail "Something went wrong!"
    Just v -> pure v

instance Stringable l => Failable (Either l) where
  x <?> t = case x of
    Left err -> case t of
      Suppress -> fail "Something went wrong!"
      Replace err' -> fail (unpack err')
      Reflect -> fail (stringify err)
    Right v -> pure v

-- Generic Utilities
skToStr :: SockAddr -> String
skToStr sockAddr = case sockAddr of
  SockAddrInet _ h4 -> show $ IPv4 $ fromHostAddress h4
  SockAddrInet6 _ _ h6 _ -> show $ IPv6 $ fromHostAddress6 h6
  SockAddrUnix _ -> error "UNIX address found on the web!"