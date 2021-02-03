{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Diagnostics
  ( -- * Diagnostic carrier
    DiagnosticsC (..),
    runDiagnostics,

    -- * Diagnostic results
    FailureBundle (..),
    ResultBundle (..),
    renderFailureBundle,
    renderWarnings,
    renderSomeDiagnostic,

    -- * Helpers
    logDiagnostic,
    logErrorBundle,
    logResultWarnings,
    logWithExit_,
    runDiagnosticsIO,
    withResult,

    -- * Re-exports
    module X,
  )
where

import Control.Carrier.Error.Either
import Control.Carrier.Reader
import Control.Carrier.Writer.Church
import Control.Effect.Diagnostics as X
import Control.Effect.Lift (sendIO, Lift)
import Control.Exception (SomeException)
import Control.Exception.Extra (safeCatch)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor (($>))
import Data.Monoid (Endo (..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Effect.Logger
import System.Exit (exitFailure)

newtype DiagnosticsC m a = DiagnosticsC {runDiagnosticsC :: ReaderC [Text] (ErrorC SomeDiagnostic (WriterC (Endo [SomeDiagnostic]) m)) a}
  deriving (Functor, Applicative, Monad, MonadIO)

data FailureBundle = FailureBundle
  { failureWarnings :: [SomeDiagnostic],
    failureCause :: SomeDiagnostic
  }

instance Show FailureBundle where
  show = show . renderFailureBundle

data ResultBundle a = ResultBundle
  { resultWarnings :: [SomeDiagnostic],
    resultValue :: a
  }

renderFailureBundle :: FailureBundle -> Doc AnsiStyle
renderFailureBundle FailureBundle {..} =
  vsep
    [ "----------",
      "A fatal error occurred:",
      "",
      indent 4 (align (renderSomeDiagnostic failureCause)),
      "",
      ">>>",
      "Relevant warnings include:",
      "",
      indent 4 (align (renderWarnings failureWarnings))
    ]

renderWarnings :: [SomeDiagnostic] -> Doc AnsiStyle
renderWarnings = align . vsep . map renderSomeDiagnostic

logResultWarnings :: Has Logger sig m => ResultBundle a -> m a
logResultWarnings ResultBundle {..} = logDebug (renderWarnings resultWarnings) $> resultValue

logErrorBundle :: Has Logger sig m => FailureBundle -> m ()
logErrorBundle = logError . renderFailureBundle

-- | Run a Diagnostic effect into a logger, using the default error/warning renderers.
logDiagnostic :: Has Logger sig m => DiagnosticsC m a -> m (Maybe a)
logDiagnostic diag = do
  result <- runDiagnostics diag
  case result of
    Left failure -> logErrorBundle failure >> pure Nothing
    Right success -> Just <$> logResultWarnings success

-- | Run a void Diagnostic effect into a logger, using the default error/warning renderers.
-- | Exits with non-zero if the result is a failure.
-- | Useful for setting up diagnostics from CLI entry points.
logWithExit_ :: (Has (Lift IO) sig m, Has Logger sig m) => DiagnosticsC m () -> m ()
logWithExit_ diag = logDiagnostic diag >>= maybe (sendIO exitFailure) pure
  
runDiagnostics :: Applicative m => DiagnosticsC m a -> m (Either FailureBundle (ResultBundle a))
runDiagnostics = fmap bundle . runWriter (\w a -> pure (appEndo w [], a)) . runError @SomeDiagnostic . runReader [] . runDiagnosticsC
  where
    bundle (warnings, res) =
      case res of
        Left err -> Left (FailureBundle warnings err)
        Right a -> Right (ResultBundle warnings a)

instance Algebra sig m => Algebra (Diagnostics :+: sig) (DiagnosticsC m) where
  alg hdl sig ctx = DiagnosticsC $ case sig of
    L (Fatal diag) -> ask >>= \path -> throwError (SomeDiagnostic path diag)
    L (Context path go) -> local (path :) $ runDiagnosticsC $ hdl (go <$ ctx)
    L (Recover act) -> do
      let -- run the action, wrapping in a Just
          go = do
            res <- runDiagnosticsC $ hdl (act <$ ctx)
            pure (Just <$> res)
          -- append the error to the warnings list and return Nothing
          errorHandler diag@(SomeDiagnostic _ _) = do
            tell (Endo (diag :))
            pure (Nothing <$ ctx)

      go `catchError` errorHandler
    R other -> alg (runDiagnosticsC . hdl) (R (R (R other))) ctx

renderSomeDiagnostic :: SomeDiagnostic -> Doc AnsiStyle
renderSomeDiagnostic (SomeDiagnostic stack cause) = renderDiagnostic cause <> line <> align (indent 2 (vsep (map (pretty . ("when " <>)) stack)))

-- | Run the DiagnosticsC carrier, also catching IO exceptions
runDiagnosticsIO :: Has (Lift IO) sig m => DiagnosticsC m a -> m (Either FailureBundle (ResultBundle a))
runDiagnosticsIO act = runDiagnostics $ act `safeCatch` (\(e :: SomeException) -> fatal e)

-- | Use the result of a Diagnostics computation, logging an error on failure
withResult :: Has Logger sig m => Severity -> Either FailureBundle (ResultBundle a) -> (a -> m ()) -> m ()
withResult sev (Left e) _ = Effect.Logger.log sev $ renderFailureBundle e
withResult _ (Right res) f = f $ resultValue res
