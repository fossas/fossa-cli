{-# LANGUAGE GADTs #-}

module Control.Effect.Finally (
  -- * Finally effect
  Finally (..),
  onExit,

  -- * Re-exports
  module X,
) where

import Control.Algebra as X
import Prelude

data Finally m k where
  OnExit :: m a -> Finally m ()

onExit :: Has Finally sig m => m a -> m ()
onExit = send . OnExit
