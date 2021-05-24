{-# LANGUAGE GADTs #-}

module Control.Effect.AtomicCounter (
  AtomicCounter (..),
  generateId,
  module X,
) where

import Control.Algebra as X

data AtomicCounter m a where
  GenerateId :: AtomicCounter m Int

generateId :: Has AtomicCounter sig m => m Int
generateId = send GenerateId
