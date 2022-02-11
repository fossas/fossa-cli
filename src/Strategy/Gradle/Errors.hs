module Strategy.Gradle.Errors (
  GradleCmdErrCtx (..),
  refGradleDocUrl,
) where

import App.Docs (strategyLangDocUrl)
import Data.List.NonEmpty (fromList)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Diag.Diagnostic (ActionableDiagCtx (..), ToDiagnostic (..), renderActionable)
import Path
import Prettyprinter (Pretty (pretty), vsep)

refGradleDocUrl :: Text
refGradleDocUrl = strategyLangDocUrl "gradle/gradle.md"

newtype GradleCmdErrCtx = GradleCmdErrCtx (Path Abs Dir)

instance ToDiagnostic GradleCmdErrCtx where
  renderDiagnostic (GradleCmdErrCtx path) =
    renderActionable $
      ActionableDiagCtx
        { description =
            vsep
              [ "Failed to run gradle, gradlew, or gradlew.bat for gradle analysis in directory:"
              , pretty $ "  " <> show path
              ]
        , documentationLinks = fromList [refGradleDocUrl]
        , troubleshootingSteps =
            Just $
              fromList $
                map
                  toText
                  [ "Ensure gradlew or gradlew.bat exists in or any parent directory of: " <> show path <> "."
                  , "Ensure gradlew or gradlew.bat exists can be executed for following commands: projects."
                  , "If not using gradle wrapper, ensure gradle executable is in your PATH."
                  ]
        }
