{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoStrictData #-}

-- | An effect that provides an atomically-modifiable state value. Similar to
-- @Control.Effect.State@
module Control.Effect.AtomicState
  ( AtomicState (..),
    modify,
    get,
    put
  )
where

import Control.Algebra
import Data.Kind

data AtomicState s (m :: Type -> Type) a where
  Modify :: (s -> (s, a)) -> AtomicState s m a

modify :: Has (AtomicState s) sig m => (s -> s) -> m ()
modify f = send (Modify (\s -> (f s, ())))

get :: Has (AtomicState s) sig m => m s
get = send (Modify (\s -> (s,s)))

put :: Has (AtomicState s) sig m => s -> m ()
put s = send (Modify (const (s,())))
