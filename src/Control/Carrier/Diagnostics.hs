{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Diagnostics (
  DiagnosticsC,
  runDiagnostics,

  -- * Helpers
  logDiagnostic,
  logWithExit_,
  runDiagnosticsIO,
  errorBoundaryIO,
  withResult,

  -- * Re-exports
  module X,
) where

import Control.Carrier.Error.Either
import Control.Carrier.Stack
import Control.Effect.Diagnostics as X
import Control.Effect.Lift (Lift, sendIO)
import Control.Exception (SomeException)
import Control.Exception.Extra (safeCatch)
import Control.Monad (unless)
import Control.Monad.Trans
import Diag.Result (Result (Failure, Success), ResultT)
import Diag.Result qualified as ResultT
import Effect.Logger
import System.Exit (exitFailure, exitSuccess)

-- TODO: ensure stack hacks are still working for e.g., inner tasks, debug bundles
newtype DiagnosticsC m a = DiagnosticsC {runDiagnosticsC :: ResultT m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

runDiagnostics :: DiagnosticsC m a -> m (ResultT.Result a)
runDiagnostics = ResultT.runResultT . runDiagnosticsC

-- FIXME: rendering of failure
-- FIXME: show warnings on success

-- | Run a Diagnostic effect into a logger, using the default error/warning renderers.
logDiagnostic :: (Has (Lift IO) sig m, Has Logger sig m, Has Stack sig m) => DiagnosticsC m a -> m (Maybe a)
logDiagnostic diag = do
  result <- runDiagnosticsIO diag
  case result of
    Failure ws eg -> logError (viaShow ws <> line <> viaShow eg) >> pure Nothing
    Success _ a -> pure $ Just a

-- result <- runDiagnosticsIO diag
-- case result of
--   Left failure -> logErrorBundle failure >> pure Nothing
--   Right success -> pure $ Just success

-- | Run a void Diagnostic effect into a logger, using the default error/warning renderers.
-- Exits with zero if the result is a success, or non-zero if the result is a failure.
-- Useful for setting up diagnostics from CLI entry points.
logWithExit_ :: (Has (Lift IO) sig m, Has Logger sig m, Has Stack sig m) => DiagnosticsC m () -> m ()
logWithExit_ diag = logDiagnostic diag >>= maybe (sendIO exitFailure) (const (sendIO exitSuccess))

instance Has Stack sig m => Algebra (Diag :+: sig) (DiagnosticsC m) where
  alg hdl sig ctx = DiagnosticsC $ case sig of
    L (FirstToSucceed ma ma') -> (ResultT.<||>) (runDiagnosticsC $ hdl (ma <$ ctx)) (runDiagnosticsC $ hdl (ma' <$ ctx))
    L (Warn w) -> (<$ ctx) <$> ResultT.warnT (ResultT.SomeWarn w)
    L (WithWarn w act) -> ResultT.withWarnT (ResultT.SomeWarn w) $ runDiagnosticsC $ hdl (act <$ ctx)
    L (ErrCtx c act) -> ResultT.errCtxT (ResultT.ErrCtx c) $ runDiagnosticsC $ hdl (act <$ ctx)
    L (Fatal diag) -> lift getStack >>= \path -> ResultT.fatalT (ResultT.Stack path) (ResultT.SomeErr diag)
    L (Recover act) -> fmap (swizzleM ctx) $ ResultT.recoverT $ runDiagnosticsC $ hdl (act <$ ctx)
    L (ErrorBoundary act) -> fmap (swizzleR ctx) $ ResultT.errorBoundaryT $ runDiagnosticsC $ hdl (act <$ ctx)
    L (Rethrow bundle) -> (<$ ctx) <$> (ResultT.ResultT (pure bundle))
    R other -> ResultT.ResultT $ thread (hdlC ~<~ hdl) other (pure ctx)

swizzleR :: Functor ctx => ctx () -> ResultT.Result (ctx a1) -> ctx (ResultT.Result a1)
swizzleR ctx (ResultT.Failure ws eg) = ResultT.Failure ws eg <$ ctx
swizzleR _ (ResultT.Success ws ctx) = ResultT.Success ws <$> ctx

swizzleM :: Functor ctx => ctx () -> Maybe (ctx a1) -> ctx (Maybe a1)
swizzleM _ (Just ctx) = Just <$> ctx
swizzleM ctx Nothing = Nothing <$ ctx

hdlC :: Applicative m => ResultT.Result (DiagnosticsC m a) -> m (ResultT.Result a)
hdlC (ResultT.Failure ws eg) = pure (ResultT.Failure ws eg)
hdlC (ResultT.Success ws m) = addWarns <$> ResultT.runResultT (runDiagnosticsC m)
  where
    addWarns (ResultT.Success ws' a') = ResultT.Success (ws' <> ws) a'
    addWarns (ResultT.Failure ws' eg') = ResultT.Failure (ws' <> ws) eg'

-- | Run the DiagnosticsC carrier, also catching IO exceptions
runDiagnosticsIO :: (Has (Lift IO) sig m, Has Stack sig m) => DiagnosticsC m a -> m (ResultT.Result a)
runDiagnosticsIO act = runDiagnostics $ act `safeCatch` (\(e :: SomeException) -> fatal e)

-- | Like 'errorBoundary', but also catches IO exceptions
errorBoundaryIO :: (Has (Lift IO) sig m, Has Diagnostics sig m) => m a -> m (ResultT.Result a)
errorBoundaryIO act = errorBoundary $ act `safeCatch` (\(e :: SomeException) -> fatal e)

-- FIXME: look at use-sites; see if warning mechanism is a better fit
-- FIXME: rendering failure
-- FIXME: displaying warnings

-- | Use the result of a Diagnostics computation, logging an error on failure
withResult :: Has Logger sig m => Severity -> Result a -> (a -> m ()) -> m ()
withResult sev (Success ws res) f = do
  unless (null ws) (Effect.Logger.log sev (viaShow ws))
  f res
withResult sev (Failure ws eg) _ = Effect.Logger.log sev (viaShow ws <> line <> viaShow eg)
