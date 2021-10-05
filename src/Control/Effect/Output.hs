{-# LANGUAGE GADTs #-}

module Control.Effect.Output (
  OutputF (..),
  Output,
  output,
  module X,
) where

import Control.Algebra as X
import Control.Carrier.Simple (Simple, sendSimple)

data OutputF o a where
  Output :: o -> OutputF o ()

type Output o = Simple (OutputF o)

output :: Has (Output o) sig m => o -> m ()
output o = sendSimple (Output o)
