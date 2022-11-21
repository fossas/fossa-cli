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

-- | The `Finally` effect runs finalizer actions at the end of a scope. These
-- actions are run even if an exception occurs during the scope.
data Finally m k where
  OnExit :: m a -> Finally m ()

onExit :: Has Finally sig m => m a -> m ()
onExit = send . OnExit
