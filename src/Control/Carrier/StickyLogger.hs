{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Exports StickyLoggerC, a carrier for the StickyLogger effect that uses
-- 'Console.Sticky.withStickyRegion' and 'Console.Sticky.setSticky' to implement
-- 'logSticky'
module Control.Carrier.StickyLogger (
  StickyLoggerC,
  runStickyLogger,
  module X,
) where

import Console.Sticky (StickyRegion, setSticky', withStickyRegion)
import Control.Carrier.Reader
import Control.Effect.Lift (Lift)
import Control.Effect.StickyLogger as X
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans)
import Effect.Logger (Logger, Severity)

runStickyLogger ::
  Has (Lift IO) sig m =>
  -- | Severity to use for log messages in ansi-incompatible terminals
  Severity ->
  StickyLoggerC m a ->
  m a
runStickyLogger sev act = withStickyRegion sev $ \region ->
  runReader region (runStickyLoggerC act)

newtype StickyLoggerC m a = StickyLoggerC {runStickyLoggerC :: ReaderC StickyRegion m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance (Has (Lift IO) sig m, Has Logger sig m) => Algebra (StickyLogger :+: sig) (StickyLoggerC m) where
  alg hdl sig ctx = StickyLoggerC $ case sig of
    L (LogSticky' msg) -> do
      region <- ask
      setSticky' region msg
      pure ctx
    R other -> alg (runStickyLoggerC . hdl) (R other) ctx
