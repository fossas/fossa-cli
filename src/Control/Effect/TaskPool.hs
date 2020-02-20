module Control.Effect.TaskPool
  ( TaskPool(..)
  , forkTask
  , module X
  ) where

import Control.Algebra as X
import Prelude

data TaskPool m k
  = forall a. ForkTask (m a) (m k)

forkTask :: Has TaskPool sig m => m a -> m ()
forkTask act = send (ForkTask act (pure ()))

instance HFunctor TaskPool where
  hmap f (ForkTask m k) = ForkTask (f m) (f k)

instance Effect TaskPool where
  thread ctx handler (ForkTask m k) = ForkTask (handler (m <$ ctx)) (handler (k <$ ctx))
