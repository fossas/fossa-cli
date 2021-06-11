{-# LANGUAGE GADTs #-}

module Control.Effect.Output (
  Output (..),
  output,
  module X,
) where

import Control.Algebra as X
import Data.Kind (Type)

data Output o (m :: Type -> Type) k where
  Output :: o -> Output o m ()

output :: Has (Output o) sig m => o -> m ()
output o = send (Output o)
