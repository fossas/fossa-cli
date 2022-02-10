-- Expose the Cancel internals for low-level usage and testing.

module Control.Timeout.Internal (
  Cancel (..),
  Duration (..),
  durationToMicro,
) where

import Control.Concurrent (MVar)

-- Opaque wrapper around MVar (sort of like an atomic variable)
-- Only created by using `timeout'`
newtype Cancel = Cancel (MVar ()) deriving (Eq)

data Duration
  = Seconds Int
  | Minutes Int
  | MilliSeconds Int
  | MicroSeconds Int
  deriving (Show)

instance Eq Duration where
  a == b = durationToMicro a == durationToMicro b

instance Ord Duration where
  compare a b = compare (durationToMicro a) (durationToMicro b)

-- threadDelay only accepts microseconds, so we simplfy that with the tiny
-- abstraction of 'Duration'.
durationToMicro :: Duration -> Int
durationToMicro = \case
  Seconds n -> n * 1_000_000
  Minutes n -> n * 60_000_000
  MilliSeconds n -> n * 1000
  MicroSeconds n -> n
