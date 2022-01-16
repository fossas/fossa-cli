module Control.Exception.Extra (
  isSyncException,
  safeCatch,
  safeTry,
) where

import Control.Effect.Exception

safeCatch :: (Exception e, Has (Lift IO) sig m) => m a -> (e -> m a) -> m a
safeCatch act hdl = act `catch` (\e -> if isSyncException e then hdl e else throwIO e)

safeTry :: (Exception e, Has (Lift IO) sig m) => m a -> m (Either e a)
safeTry act = (Right <$> act) `safeCatch` (pure . Left)

isSyncException :: Exception e => e -> Bool
isSyncException e =
  case fromException (toException e) of
    Just (SomeAsyncException _) -> False
    Nothing -> True
