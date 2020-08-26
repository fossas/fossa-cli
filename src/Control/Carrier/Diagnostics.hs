{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Diagnostics
  ( -- * Diagnostic carrier
    DiagnosticsC (..),
    runDiagnostics,

    -- * Diagnostic results
    FailureBundle (..),
    renderFailureBundle,
    renderWarnings,
    renderSomeDiagnostic,
    ResultBundle (..),

    -- * Re-exports
    module X,
  )
where

import Control.Carrier.Error.Either
import Control.Carrier.Reader
import Control.Carrier.Writer.Church
import Control.Effect.Diagnostics as X
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid (Endo (..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Prelude

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
