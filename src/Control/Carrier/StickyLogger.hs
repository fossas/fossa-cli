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

import Console.Sticky (setSticky', withStickyRegion)
import Control.Carrier.Reader
import Control.Carrier.Simple
import Control.Effect.Lift (Lift)
import Control.Effect.StickyLogger as X
import Effect.Logger (Logger, Severity)

runStickyLogger ::
  (Has (Lift IO) sig m, Has Logger sig m) =>
  -- | Severity to use for log messages in ansi-incompatible terminals
  Severity ->
  StickyLoggerC m a ->
  m a
runStickyLogger sev act = withStickyRegion sev $ \region ->
  interpret
    ( \case
        LogSticky' msg -> do
          setSticky' region msg
    )
    act

type StickyLoggerC = SimpleC StickyLogger
