module Trans where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)
import Servant (Handler)

newtype HandlerT a = HandlerT {runHandlerT :: Handler (Either Text a)}

liftHT :: Either Text a -> HandlerT a
liftHT v = HandlerT $ pure v

instance MonadIO HandlerT where
  liftIO = liftIO

instance Functor HandlerT where
  fmap f (HandlerT val) = HandlerT $ do
    val >>= \case
      Left err -> pure (Left err)
      Right v -> pure (Right (f v))

instance Applicative HandlerT where
  pure x = HandlerT (pure (Right x))
  HandlerT fn <*> HandlerT v = HandlerT $ do
    fn <- fn
    v <- v
    pure $ fn <*> v

instance Monad HandlerT where
  (HandlerT val) >>= fn = HandlerT $ do
    val >>= \case
      Left err -> pure (Left err)
      Right v -> runHandlerT $ fn v
