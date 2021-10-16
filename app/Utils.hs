{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils where

import Data.Text (Text, unpack)
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
class Monad m => Failable m where
  (<?>) :: m a -> Text -> HandlerT a
  (<!>) :: m a -> Bool -> HandlerT a

instance Failable Maybe where
  x <?> t = case x of
    Nothing -> fail (unpack t)
    Just v -> pure v
  x <!> b = x <?> "Something went wrong!"

instance Stringable l => Failable (Either l) where
  x <?> t = case x of
    Left err -> fail (unpack t)
    Right v -> pure v
  x <!> b = case x of
    Left err -> if b then fail "Something went wrong!" else fail (stringify err)
    Right v -> pure v
