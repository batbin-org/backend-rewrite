module Trans where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Servant (Handler)

newtype HandlerT e a = HandlerT {runHandlerT :: Handler (Either e a)}

liftHT :: Either e a -> HandlerT e a
liftHT v = HandlerT $ pure v

instance MonadIO (HandlerT e) where
  liftIO = liftIO

instance Functor (HandlerT e) where
  fmap f (HandlerT val) = HandlerT $ do
    val >>= \case
      Left err -> pure (Left err)
      Right v -> pure (Right (f v))

instance Applicative (HandlerT e) where
  pure x = HandlerT (pure (Right x))
  HandlerT fn <*> HandlerT v = HandlerT $ do
    fn <- fn
    v <- v
    pure $ fn <*> v

instance Monad (HandlerT e) where
  (HandlerT val) >>= fn = HandlerT $ do
    val >>= \case
      Left err -> pure (Left err)
      Right v -> runHandlerT $ fn v
