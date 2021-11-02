module Trans where

import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import Data.Text (Text, pack)
import Servant (Handler)
import Types (Status (Status))

newtype HandlerT a = HandlerT {runHandlerT :: Handler (Either Status a)}

succeed :: Text -> HandlerT Status
succeed t = pure (Status True t)

liftHT :: Either Status a -> HandlerT a
liftHT = HandlerT . pure

instance MonadIO HandlerT where
  liftIO action = HandlerT $ liftIO action <&> Right

instance MonadFail HandlerT where
  fail msg = HandlerT (pure (Left (Status False (pack msg))))

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
