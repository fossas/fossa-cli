module Control.Effect.Output
  ( Output(..)
  , output
  , module X
  ) where

import Control.Algebra as X

data Output o m k where
  Output :: o -> Output o m ()

output :: Has (Output o) sig m => o -> m ()
output o = send (Output o)
