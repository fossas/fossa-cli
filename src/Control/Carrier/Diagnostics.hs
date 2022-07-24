{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Diagnostics (
  DiagnosticsC,
  runDiagnostics,

  -- * Helpers
  logWithExit_,
  runDiagnosticsIO,
  errorBoundaryIO,
  withResult,
  flushLogs,

  -- * Re-exports
  module X,
) where

import Control.Carrier.Error.Either
import Control.Carrier.Stack
import Control.Effect.Diagnostics as X
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Telemetry (Telemetry, trackResult)
import Control.Exception (SomeException)
import Control.Exception.Extra (safeCatch)
import Control.Monad (void)
import Control.Monad.Trans
import Data.Foldable (traverse_)
import Diag.Monad (ResultT)
import Diag.Monad qualified as ResultT
import Diag.Result (Result (Failure, Success), renderFailure, renderSuccess)
import Diag.Result qualified as Result
import Effect.Logger
import System.Exit (exitFailure, exitSuccess)

newtype DiagnosticsC m a = DiagnosticsC {runDiagnosticsC :: ResultT m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

runDiagnostics :: DiagnosticsC m a -> m (Result a)
runDiagnostics = ResultT.runResultT . runDiagnosticsC

-- | Run a Diagnostic effect into a logger, using the default error/warning renderers.
logDiagnostic :: (Has (Lift IO) sig m, Has Logger sig m, Has Stack sig m, Has Telemetry sig m) => DiagnosticsC m a -> m (Maybe a)
logDiagnostic diag = do
  result <- runDiagnosticsIO diag
  trackResult result
  case result of
    Failure ws eg -> logError (renderFailure ws eg "An issue occurred") >> pure Nothing
    Success ws a -> do
      traverse_ logWarn (renderSuccess ws "A task succeeded with warnings")
      pure (Just a)

-- | Run a void Diagnostic effect into a logger, using the default error/warning renderers.
-- Exits with zero if the result is a success, or non-zero if the result is a failure.
-- Useful for setting up diagnostics from CLI entry points.
logWithExit_ :: (Has (Lift IO) sig m, Has Logger sig m, Has Stack sig m, Has Telemetry sig m) => DiagnosticsC m () -> m ()
logWithExit_ diag = logDiagnostic diag >>= maybe (sendIO exitFailure) (const (sendIO exitSuccess))

instance Has Stack sig m => Algebra (Diag :+: sig) (DiagnosticsC m) where
  alg hdl sig ctx = DiagnosticsC $ case sig of
    L (FirstToSucceed ma ma') ->
      (ResultT.<||>)
        (runDiagnosticsC $ hdl (ma <$ ctx))
        (runDiagnosticsC $ hdl (ma' <$ ctx))
    L (Warn w) ->
      (<$ ctx) <$> ResultT.warnT (Result.SomeWarn w)
    L (WarnOnErr w act) ->
      ResultT.warnOnErrT (Result.SomeWarn w) $ runDiagnosticsC $ hdl (act <$ ctx)
    L (ErrCtx c act) ->
      ResultT.errCtxT (Result.ErrCtx c) $ runDiagnosticsC $ hdl (act <$ ctx)
    L (Fatal diag) -> do
      stack <- lift getStack
      ResultT.fatalT (Result.Stack stack) (Result.SomeErr diag)
    L (Recover act) ->
      fmap (distributeMaybe ctx) $
        ResultT.recoverT $
          runDiagnosticsC $
            hdl (act <$ ctx)
    L (ErrorBoundary act) ->
      fmap (distributeResult ctx) $
        ResultT.errorBoundaryT $
          runDiagnosticsC $
            hdl (act <$ ctx)
    L (Rethrow result) ->
      (<$ ctx) <$> ResultT.rethrowT result
    R other ->
      ResultT.ResultT $ thread (hdlResult ~<~ hdl) other (pure ctx)
  {-# INLINE alg #-}

-- "invert" a Result into a functorial context
distributeResult :: Functor ctx => ctx () -> Result (ctx a) -> ctx (Result a)
distributeResult ctx (Failure ws eg) = Failure ws eg <$ ctx
distributeResult _ (Success ws ctx) = Success ws <$> ctx
{-# INLINE distributeResult #-}

-- "invert" a Maybe into a functorial context
distributeMaybe :: Functor ctx => ctx () -> Maybe (ctx a) -> ctx (Maybe a)
distributeMaybe _ (Just ctx) = Just <$> ctx
distributeMaybe ctx Nothing = Nothing <$ ctx
{-# INLINE distributeMaybe #-}

-- "distribute" a Result into a DiagnosticsC computation
hdlResult :: Applicative m => Result (DiagnosticsC m a) -> m (Result a)
hdlResult (Failure ws eg) = pure (Failure ws eg)
hdlResult (Success ws m) = addWarns <$> runDiagnostics m
  where
    addWarns (Success ws' a') = Success (ws' <> ws) a'
    addWarns (Failure ws' eg') = Failure (ws' <> ws) eg'
{-# INLINE hdlResult #-}

-- | Run the DiagnosticsC carrier, also catching IO exceptions
runDiagnosticsIO :: (Has (Lift IO) sig m, Has Stack sig m) => DiagnosticsC m a -> m (Result a)
runDiagnosticsIO act = runDiagnostics $ act `safeCatch` (\(e :: SomeException) -> fatal e)

-- | Like 'errorBoundary', but also catches IO exceptions
errorBoundaryIO :: (Has (Lift IO) sig m, Has Diagnostics sig m) => m a -> m (Result a)
errorBoundaryIO act = errorBoundary $ act `safeCatch` (\(e :: SomeException) -> fatal e)

-- | Use the result of a Diagnostics computation, logging any encountered errors
-- and warnings
--
-- - On failure, the failure is logged with the provided @sevOnErr@ severity
--
-- - On success, the associated warnings are logged with the provided
--   @sevOnSuccess@ severity
withResult :: Has Logger sig m => Severity -> Severity -> Result a -> (a -> m ()) -> m ()
withResult sevOnErr _ (Failure ws eg) _ = Effect.Logger.log sevOnErr (renderFailure ws eg "An issue occurred")
withResult _ sevOnSuccess (Success ws res) f = do
  traverse_
    (Effect.Logger.log sevOnSuccess)
    (renderSuccess ws "A task succeeded with warnings")
  f res

-- | Log all encountered errors and warnings associated with 'Result a'
--
-- - On failure, the failure is logged with the provided @sevOnErr@ severity
--
-- - On success, the associated warnings are logged with the provided
--   @sevOnSuccess@ severity
flushLogs :: Has Logger sig m => Severity -> Severity -> Result a -> m ()
flushLogs s1 s2 res = withResult s1 s2 res (void . pure)
