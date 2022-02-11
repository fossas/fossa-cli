module Strategy.Dart.Errors (
  PubspecLimitation (..),
  refPubDocUrl,
) where

import App.Docs (strategyLangDocUrl)
import Data.List.NonEmpty (fromList)
import Data.Text (Text)
import Diag.Diagnostic (ActionableDiagCtx (..), ToDiagnostic (..), renderActionable)

refPubDocUrl :: Text
refPubDocUrl = strategyLangDocUrl "dart/dart.md"

data PubspecLimitation = PubspecLimitation

instance ToDiagnostic PubspecLimitation where
  renderDiagnostic (PubspecLimitation) =
    renderActionable $
      ActionableDiagCtx
        { description = "Suboptimal pubspec.yaml analysis performed - Edges and deep dependencies will not be reported."
        , documentationLinks = fromList [refPubDocUrl]
        , troubleshootingSteps =
            Just $
              fromList
                [ "Build your project and ensure pubspec.lock file exists prior to using fossa-cli."
                , "Ensure dart or flutter executables are in PATH."
                ]
        }
