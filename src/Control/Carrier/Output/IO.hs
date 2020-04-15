module Control.Carrier.Output.IO
  ( OutputC(..)
  , runOutput
  , module X
  ) where

import Control.Effect.Output as X
import Control.Carrier.Reader
import Data.IORef
import Prologue

runOutput :: forall o m a. MonadIO m => OutputC o m a -> m ([o],a)
runOutput act = do
  ref <- liftIO $ newIORef []
  res <- runReader ref (runOutputC act)
  outputs <- liftIO $ readIORef ref
  pure (outputs, res)

newtype OutputC o m a = OutputC { runOutputC :: ReaderC (IORef [o]) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadTrans)

instance (Algebra sig m, MonadIO m) => Algebra (Output o :+: sig) (OutputC o m) where
  alg (L (Output o k)) = do
    ref <- OutputC ask
    liftIO (atomicModifyIORef' ref (\xs -> ((o:xs),()))) *> k
  alg (R other) = OutputC (alg (R (handleCoercible other)))
