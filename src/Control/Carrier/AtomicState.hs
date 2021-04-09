{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A carrier for the AtomicState effect that utilizes an 'IORef' for atomic
-- state updates
module Control.Carrier.AtomicState
  ( AtomicStateC (AtomicStateC),
    runAtomicState,
    module X,
  )
where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Effect.AtomicState as X
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans)
import Data.IORef

newtype AtomicStateC s m a = AtomicStateC {runAtomicStateC :: ReaderC (IORef s) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

runAtomicState :: Has (Lift IO) sig m => s -> AtomicStateC s m a -> m (s, a)
runAtomicState s act = do
  ref <- sendIO $ newIORef s
  res <- runReader ref $ runAtomicStateC act
  final <- sendIO $ readIORef ref
  pure (final, res)

instance Has (Lift IO) sig m => Algebra (AtomicState s :+: sig) (AtomicStateC s m) where
  alg hdl sig ctx = AtomicStateC $ do
    case sig of
      L (Modify f) -> do
        ref <- ask @(IORef s)
        res <- sendIO $ atomicModifyIORef' ref f
        pure (res <$ ctx)
      R other -> alg (runAtomicStateC . hdl) (R other) ctx
