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
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.IORef
import Prelude
import Control.Monad.IO.Unlift (MonadUnliftIO)

newtype FinallyC m a = FinallyC {runFinallyC :: ReaderC (IORef [FinallyC m ()]) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

runFinally :: Has (Lift IO) sig m => FinallyC m a -> m a
runFinally (FinallyC go) = do
  ref <- sendIO $ newIORef []
  runReader ref go `finally` (traverse_ runFinally =<< sendIO (readIORef ref))

instance (Has (Lift IO) sig m, Algebra sig m) => Algebra (Finally :+: sig) (FinallyC m) where
  alg hdl sig ctx = FinallyC $ case sig of
    L (OnExit go) -> do
      ref <- ask
      sendIO $ atomicModifyIORef' ref (\cur -> (void (hdl (go <$ ctx)) : cur, ()))
      pure ctx
    R other -> alg (runFinallyC . hdl) (R other) ctx
