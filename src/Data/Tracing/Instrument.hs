module Data.Tracing.Instrument (
  CounterRegistry,
  mkCounterRegistry,
  getCounterRegistry,
  getCountOf,
  incCount,
) where

import Control.Concurrent.STM (
  STM,
  TVar,
  newTVar,
  readTVar,
  writeTVar,
 )
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

-- Thread-safe atomic counter registry.
type CounterRegistry a b = TVar (Map.Map a b)

-- | Make empty counter registry.
mkCounterRegistry :: Ord a => STM (CounterRegistry a b)
mkCounterRegistry = newTVar mempty

-- | Get counter registry.
getCounterRegistry :: CounterRegistry a b -> STM (Map.Map a b)
getCounterRegistry = readTVar

-- | If counter exists in registry, returns current count, otherwise returns Nothing.
getCountOf :: Ord a => CounterRegistry a b -> a -> STM (Maybe b)
getCountOf counter key = do
  Map.lookup key <$> readTVar counter

-- | Increases a count by 1 for given counter.
-- If counter does not exist within registry, it initialized counter with value of 1.
incCount :: (Ord a, Num b) => a -> CounterRegistry a b -> STM ()
incCount key = withCounterRegistry key 0 (+ 1)

withCounterRegistry :: (Ord a) => a -> b -> (b -> b) -> CounterRegistry a b -> STM ()
withCounterRegistry key initValue f counter = do
  currentCounterRegistry <- getCounterRegistry counter
  currentCount <- fromMaybe initValue <$> getCountOf counter key
  writeTVar counter (Map.insert key (f currentCount) currentCounterRegistry)
