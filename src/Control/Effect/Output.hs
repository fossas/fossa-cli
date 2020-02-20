module Control.Effect.Output
  ( Output(..)
  , output
  , module X
  ) where

import Control.Algebra as X
import Prologue

data Output o m k
  = Output o (m k)
  deriving Generic1

instance HFunctor (Output o)
instance Effect (Output o)

output :: Has (Output o) sig m => o -> m ()
output o = send (Output o (pure ()))
