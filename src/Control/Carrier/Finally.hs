{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Finally (
  -- * Finally carrier
  FinallyC (..),
  runFinally,

  -- * Re-exports
  module X,
) where

import Control.Carrier.Reader
import Control.Effect.Exception (finally)
import Control.Effect.Finally as X
import Control.Effect.Lift
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans (MonadTrans (..))
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.IORef

-- | `FinallyC` implements `Finally` by carrying around an `IORef` containing a
-- list of finalizer actions.
newtype FinallyC m a = FinallyC {runFinallyC :: ReaderC (IORef [FinallyC m ()]) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance MonadTrans (FinallyC) where
  lift = FinallyC . lift

instance MonadError e m => MonadError e (FinallyC m) where
  throwError = lift . throwError
  catchError action handler =
    FinallyC . ReaderC $ \ref ->
      catchError (runReader ref . runFinallyC $ action) (runReader ref . runFinallyC . handler)

-- | `runFinally` interprets a `FinallyC` by running the underlying effect
--  stack, then using `finally` to run all registered finalizers.
runFinally :: Has (Lift IO) sig m => FinallyC m a -> m a
runFinally (FinallyC go) = do
  ref <- sendIO $ newIORef []
  -- Note that we `traverse_ runFinally` because finalizers run in the full
  -- effect stack (including `Finally`), and can possibly register their own
  -- finalizers.
  runReader ref go `finally` (traverse_ runFinally =<< sendIO (readIORef ref))

instance (Has (Lift IO) sig m, Algebra sig m) => Algebra (Finally :+: sig) (FinallyC m) where
  alg hdl sig ctx = FinallyC $ case sig of
    -- Register a new finalizer `OnExit` by grabbing the IORef from the Reader
    -- and cons'ing the new finalizer.
    L (OnExit go) -> do
      ref <- ask
      -- Note that we re-wrap the finalizer using `hdl` so that finalizers can
      -- run in the full effect stack.
      sendIO $ atomicModifyIORef' ref (\cur -> (void (hdl (go <$ ctx)) : cur, ()))
      pure ctx
    -- Run `FinallyC` into a `ReaderC`, and run the rest of the effect stack.
    R other -> alg (runFinallyC . hdl) (R other) ctx
