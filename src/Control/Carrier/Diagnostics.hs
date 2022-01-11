{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Diagnostics (
  DiagnosticsC,
  runDiagnostics,

  -- * DiagErr carrier
  DiagErrC,
  runDiagErr,

  -- * Helpers
  logDiagnostic,
  logErrorBundle,
  logWithExit_,
  runDiagnosticsIO,
  errorBoundaryIO,
  withResult,

  -- * Re-exports
  module X,
) where

import Control.Carrier.Error.Either
import Control.Carrier.Stack
import Control.Carrier.Writer.Church
import Control.Effect.Diagnostics as X
import Control.Effect.Lift (Lift, sendIO)
import Control.Exception (SomeException)
import Control.Exception.Extra (safeCatch)
import Control.Monad.Trans
import Data.Foldable (traverse_)
import Data.Monoid (Endo (..))
import Effect.Logger
import System.Exit (exitFailure, exitSuccess)

newtype DiagnosticsC m a = DiagnosticsC {runDiagnosticsC :: DiagErrC (StackC m) a}
  deriving (Functor, Applicative, Monad, MonadIO, Algebra (DiagErr :+: Stack :+: sig))

--deriving instance Algebra sig m => Algebra (DiagErr :+: Stack :+: sig) (DiagnosticsC m)

runDiagnostics :: Applicative m => DiagnosticsC m a -> m (Either FailureBundle a)
runDiagnostics = runStack [] . runDiagErr . runDiagnosticsC

instance MonadTrans DiagnosticsC where
  lift = DiagnosticsC . lift . lift

newtype DiagErrC m a = DiagErrC {runDiagErrC :: ErrorC SomeDiagnostic (WriterC (Endo [SomeDiagnostic]) m) a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans DiagErrC where
  lift = DiagErrC . lift . lift

logErrorBundle :: Has Logger sig m => FailureBundle -> m ()
logErrorBundle = logError . renderFailureBundle

-- | Run a Diagnostic effect into a logger, using the default error/warning renderers.
logDiagnostic :: (Has (Lift IO) sig m, Has Logger sig m) => DiagnosticsC m a -> m (Maybe a)
logDiagnostic diag = do
  result <- runDiagnosticsIO diag
  case result of
    Left failure -> logErrorBundle failure >> pure Nothing
    Right success -> pure $ Just success

-- | Run a void Diagnostic effect into a logger, using the default error/warning renderers.
-- Exits with zero if the result is a success, or non-zero if the result is a failure.
-- Useful for setting up diagnostics from CLI entry points.
logWithExit_ :: (Has (Lift IO) sig m, Has Logger sig m) => DiagnosticsC m () -> m ()
logWithExit_ diag = logDiagnostic diag >>= maybe (sendIO exitFailure) (const (sendIO exitSuccess))

runDiagErr :: Applicative m => DiagErrC m a -> m (Either FailureBundle a)
runDiagErr = fmap bundle . runWriter (\w a -> pure (appEndo w [], a)) . runError @SomeDiagnostic . runDiagErrC
  where
    bundle (warnings, res) =
      case res of
        Left err -> Left (FailureBundle warnings err)
        Right a -> Right a

instance Has Stack sig m => Algebra (DiagErr :+: sig) (DiagErrC m) where
  alg hdl sig ctx = DiagErrC $ case sig of
    L (Fatal diag) -> getStack >>= \path -> throwError (SomeDiagnostic path diag)
    L (Recover' act) -> do
      let -- run the action, wrapping in a Right
          go = do
            res <- runDiagErrC $ hdl (act <$ ctx)
            pure (Right <$> res)
          -- append the error to the warnings list and return Left
          errorHandler diag@(SomeDiagnostic _ _) = do
            tell (Endo (diag :))
            pure (Left diag <$ ctx)

      go `catchError` errorHandler
    L (ErrorBoundary act) -> do
      let act' = runDiagErr $ hdl (act <$ ctx)

      -- have to lift for each inner monad transformer (reader, error, writer)
      res' <- lift . lift $ act'
      case res' of
        Left e -> pure (Left e <$ ctx)
        Right a -> pure (Right <$> a)
    L (Rethrow bundle) -> do
      traverse_ (\diag -> tell (Endo (diag :))) (failureWarnings bundle)
      throwError (failureCause bundle)
    R other -> alg (runDiagErrC . hdl) (R (R other)) ctx

-- | Run the DiagnosticsC carrier, also catching IO exceptions
runDiagnosticsIO :: Has (Lift IO) sig m => DiagnosticsC m a -> m (Either FailureBundle a)
runDiagnosticsIO act = runDiagnostics $ act `safeCatch` (\(e :: SomeException) -> fatal e)

-- | Like 'errorBoundary', but also catches IO exceptions
errorBoundaryIO :: (Has (Lift IO) sig m, Has Diagnostics sig m) => m a -> m (Either FailureBundle a)
errorBoundaryIO act = errorBoundary $ act `safeCatch` (\(e :: SomeException) -> fatal e)

-- | Use the result of a Diagnostics computation, logging an error on failure
withResult :: Has Logger sig m => Severity -> Either FailureBundle a -> (a -> m ()) -> m ()
withResult sev (Left e) _ = Effect.Logger.log sev $ renderFailureBundle e
withResult _ (Right res) f = f res
