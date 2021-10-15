{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Output.IO (
  OutputC,
  runOutput,
  module X,
) where

import Control.Carrier.Reader
import Control.Carrier.Simple
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Output as X
import Data.IORef

type OutputC o = SimpleC (OutputF o)

runOutput :: forall o sig m a. Has (Lift IO) sig m => OutputC o m a -> m ([o], a)
runOutput act = do
  ref <- sendIO $ newIORef []
  res <- runOutputRef ref act
  outputs <- sendIO $ readIORef ref
  pure (reverse outputs, res)

runOutputRef :: Has (Lift IO) sig m => IORef [o] -> OutputC o m a -> m a
runOutputRef ref = interpret $ \case
  Output o -> sendIO (atomicModifyIORef' ref (\xs -> (o : xs, ())))
