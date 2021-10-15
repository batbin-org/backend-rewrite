module Trans where

import Control.Exception (catch)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Servant (Handler)
import Types (Status (Status))

newtype HandlerT a = HandlerT {runHandlerT :: Handler (Either Status a)}

liftHT :: Either Status a -> HandlerT a
liftHT v = HandlerT $ pure v

failHT :: Text -> HandlerT Status
failHT err = HandlerT $ pure (Left $ Status False err)

instance MonadIO HandlerT where
  liftIO = liftIO

instance Functor HandlerT where
  fmap f (HandlerT val) =
    HandlerT $
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
  (HandlerT val) >>= fn =
    HandlerT $
      val >>= \case
        Left err -> pure (Left err)
        Right v -> runHandlerT $ fn v
