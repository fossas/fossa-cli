{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.DiagWarn (
  -- * DiagWarn operations
  warn,
  withWarn,
  DiagWarn (..),

  -- * TEMPORARY: A logging carrier for warnings
  LogWarningsC (..),
  runDiagWarn,

  REPLACEME(..),

  -- * Re-exports
  module X,
) where

import Control.Algebra as X
import Control.Effect.Diagnostics (ToDiagnostic, renderDiagnostic, Diagnostics, errorBoundary, rethrow, renderFailureBundle)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans, lift)
import Effect.Logger

-- | An effect for emitting warnings
data DiagWarn m a where
  -- TODO: use specific "warning" type, instead of using ToDiagnostic?
  Warn :: ToDiagnostic diag => diag -> DiagWarn m ()

-- | Emit a warning
warn :: (ToDiagnostic diag, Has DiagWarn sig m) => diag -> m ()
warn = send . Warn

-- | Contextualize a thrown error with a warning
--
-- FIXME: this doesn't currently use the provided diagnostic, and just logs the failure bundle (to maintain present behavior)
withWarn :: (Has Logger sig m, Has Diagnostics sig m) => diag -> m a -> m a
withWarn _ m = do
  maybeBundle <- errorBoundary m
  case maybeBundle of
    Left bundle -> do
      logWarn (renderFailureBundle bundle)
      rethrow bundle
    Right res -> pure res

---------- FIXME: a dummy warning to allow 'withWarn' invocations to compile

data REPLACEME = REPLACEME

---------- FIXME: temporary shim to log warnings (to maintain present behavior)

newtype LogWarningsC m a = LogWarningsC {unLogWarningsC :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans LogWarningsC where
  lift = LogWarningsC

runDiagWarn :: LogWarningsC m a -> m a
runDiagWarn = unLogWarningsC

instance Has Logger sig m => Algebra (DiagWarn :+: sig) (LogWarningsC m) where
  alg hdl sig ctx = LogWarningsC $ case sig of
    L (Warn diag) -> do
      logWarn (renderDiagnostic diag)
      pure ctx
    R other -> alg (unLogWarningsC . hdl) other ctx
