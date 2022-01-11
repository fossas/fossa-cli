{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.DiagWarn (
  -- * DiagWarn operations
  warn,
  DiagWarn (..),

  -- * A logging carrier for warnings
  LogWarningsC (..),
  runDiagWarn,
  module X,
) where

import Control.Algebra as X
import Control.Effect.Diagnostics (ToDiagnostic, renderDiagnostic)
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
