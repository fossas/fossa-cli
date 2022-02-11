module Strategy.Gradle.Errors (
  GradleCmdErrCtx (..),
  FailedToListProjects (..),
  refGradleDocUrl,
) where

import App.Docs (strategyLangDocUrl)
import Data.List.NonEmpty (fromList)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Diag.Diagnostic (ActionableDiagCtx (..), ToDiagnostic (..), renderActionable)
import Effect.Logger (viaShow)
import Path
import Prettyprinter (Pretty (pretty), vsep)

refGradleDocUrl :: Text
refGradleDocUrl = strategyLangDocUrl "gradle/gradle.md"

newtype FailedToListProjects = FailedToListProjects (Path Abs Dir) deriving (Eq, Ord, Show)
instance ToDiagnostic FailedToListProjects where
  renderDiagnostic (FailedToListProjects dir) =
    renderActionable $
      ActionableDiagCtx
        { description = "Found a gradle build manifest in " <> viaShow dir <> " but, could not list projects."
        , documentationLinks = fromList [refGradleDocUrl]
        , troubleshootingSteps =
            Just $
              fromList
                [ "Ensure gradle, gradlew or gradlew.bat can execute projects subcommand."
                ]
        }

newtype GradleCmdErrCtx = GradleCmdErrCtx (Path Abs Dir) deriving (Eq, Ord, Show)
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
                  , "If not using gradle wrapper, ensure gradle executable is in your PATH."
                  ]
        }
