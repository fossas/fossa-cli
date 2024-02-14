module Strategy.Dart.Errors (
  PubspecLimitation (..),
  refPubDocUrl,
) where

import App.Docs (strategyLangDocUrl)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic (..))
import Errata (Errata (..))

refPubDocUrl :: Text
refPubDocUrl = strategyLangDocUrl "dart/dart.md"

data PubspecLimitation
  = PubspecLimitationCtx
  | PubspecLimitationHelp

instance ToDiagnostic PubspecLimitation where
  renderDiagnostic PubspecLimitationCtx = do
    let header = "Could not perform analysis using lockfile"
    Errata (Just header) [] Nothing
  renderDiagnostic PubspecLimitationHelp = do
    let header = "Build your project and ensure pubspec.lock file exists and is readable prior to using fossa-cli"
    Errata (Just header) [] Nothing
