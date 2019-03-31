module Control.Monad.Throw.Extra
  ( maybeThrow
  , eitherThrow
  ) where

import Control.Monad.Catch (MonadThrow, throwM)

maybeThrow
  :: MonadThrow m
  => Exception e
  => e
  -> Maybe a
  -> m a
maybeThrow e = maybe (throwM e) return

eitherThrow
  :: MonadThrow m
  => Exception ex
  => (e -> ex)
  -> Either e a
  -> m a
eitherThrow f = either (throwM . f) return
