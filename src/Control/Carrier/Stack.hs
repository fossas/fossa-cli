{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Stack (
  -- * Stack carrier
  StackC,
  runStack,
  runStackWith,

  -- * Re-exports
  module X,
) where

import Control.Carrier.Reader
import Control.Effect.Stack as X
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Text (Text)

newtype StackC m a = StackC {runStackC :: ReaderC [Text] m a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run the Stack effect with an empty initial callstack
runStack :: StackC m a -> m a
runStack = runStackWith []

-- | Run the Stack effect with the provided initial callstack
runStackWith :: [Text] -> StackC m a -> m a
runStackWith initial = runReader initial . runStackC

instance MonadTrans StackC where
  lift = StackC . lift

instance Algebra sig m => Algebra (Stack :+: sig) (StackC m) where
  alg hdl sig ctx = StackC $ case sig of
    L (Context s m) -> do
      let lowered = runStackC (hdl (m <$ ctx))
      local (s :) lowered
    L GetStack -> (<$ ctx) <$> ask
    L (WithEmptyStack m) -> do
      let lowered = runStackC (hdl (m <$ ctx))
      lift (runReader [] lowered)
    R other -> alg (runStackC . hdl) (R other) ctx
