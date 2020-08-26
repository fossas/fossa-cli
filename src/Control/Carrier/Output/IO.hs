{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Output.IO
  ( OutputC (..),
    runOutput,
    module X,
  )
where

import Control.Applicative (Alternative)
import Control.Carrier.Reader
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Output as X
import Control.Monad.IO.Class
import Control.Monad.Trans (MonadTrans)
import Data.IORef

runOutput :: forall o sig m a. Has (Lift IO) sig m => OutputC o m a -> m ([o], a)
runOutput act = do
  ref <- sendIO $ newIORef []
  res <- runReader ref (runOutputC act)
  outputs <- sendIO $ readIORef ref
  pure (outputs, res)

newtype OutputC o m a = OutputC {runOutputC :: ReaderC (IORef [o]) m a}
  deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadFail, MonadTrans)

instance Has (Lift IO) sig m => Algebra (Output o :+: sig) (OutputC o m) where
  alg hdl sig ctx = OutputC $ case sig of
    L (Output o) -> do
      ref <- ask
      sendIO (atomicModifyIORef' ref (\xs -> ((o : xs), ())))
      pure ctx
    R other -> alg (runOutputC . hdl) (R other) ctx
