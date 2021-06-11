{-# LANGUAGE GADTs #-}

module Control.Effect.TaskPool (
  TaskPool (..),
  forkTask,
  module X,
) where

import Control.Algebra as X

data TaskPool m k where
  ForkTask :: m a -> TaskPool m ()

forkTask :: Has TaskPool sig m => m a -> m ()
forkTask act = send (ForkTask act)
