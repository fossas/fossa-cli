module Strategy.Dart.Errors (
  PubspecLimitation (..),
  refPubDocUrl,
) where

import App.Docs (strategyLangDocUrl)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic (..))
import Prettyprinter (Pretty (pretty), indent, vsep)

refPubDocUrl :: Text
refPubDocUrl = strategyLangDocUrl "dart/dart.md"

data PubspecLimitation = PubspecLimitation

instance ToDiagnostic PubspecLimitation where
  renderDiagnostic (PubspecLimitation) =
    vsep
      [ "Could not perform analysis using lockfile."
      , ""
      , "Build your project and ensure pubspec.lock file exists and is readable prior to using fossa-cli."
      , ""
      , "Refer to:"
      , indent 2 $ pretty $ "- " <> refPubDocUrl
      ]
