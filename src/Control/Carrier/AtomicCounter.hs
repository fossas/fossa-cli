{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.AtomicCounter (
  AtomicCounterC,
  runAtomicCounter,

  -- * Re-exports
  module X,
) where

import Control.Carrier.AtomicState
import Control.Effect.AtomicCounter as X
import Control.Effect.Lift
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans)

runAtomicCounter :: Has (Lift IO) sig m => AtomicCounterC m a -> m a
runAtomicCounter = fmap snd . runAtomicState 1 . runAtomicCounterC

newtype AtomicCounterC m a = AtomicCounterC {runAtomicCounterC :: AtomicStateC Int m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance Has (Lift IO) sig m => Algebra (AtomicCounter :+: sig) (AtomicCounterC m) where
  alg hdl sig ctx = AtomicCounterC $ case sig of
    L GenerateId -> do
      generated <- getSet @Int (\old -> (old + 1, old))
      pure (generated <$ ctx)
    R other -> alg (runAtomicCounterC . hdl) (R other) ctx
