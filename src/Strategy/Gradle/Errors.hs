module Strategy.Gradle.Errors (
  GradleCmdErrCtx (..),
  FailedToListProjects (..),
  refGradleDocUrl,
) where

import App.Docs (strategyLangDocUrl)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Effect.Logger (viaShow)
import Path
import Prettyprinter (Pretty (pretty), indent, vsep)

refGradleDocUrl :: Text
refGradleDocUrl = strategyLangDocUrl "gradle/gradle.md"

newtype FailedToListProjects = FailedToListProjects (Path Abs Dir) deriving (Eq, Ord, Show)
instance ToDiagnostic FailedToListProjects where
  renderDiagnostic (FailedToListProjects dir) =
    vsep
      [ "Found a gradle build manifest in " <> viaShow dir <> " but, could not list projects."
      ]

newtype GradleCmdErrCtx = GradleCmdErrCtx (Path Abs Dir) deriving (Eq, Ord, Show)
instance ToDiagnostic GradleCmdErrCtx where
  renderDiagnostic (GradleCmdErrCtx path) =
    vsep
      [ "Failed to run gradle, gradlew, or gradlew.bat for gradle analysis in the directory:"
      , indent 2 $ pretty . show $ path
      , ""
      , indent 2 $ pretty $ "Ensure gradlew or gradlew.bat exists in or in the parent directory of: " <> show path <> "."
      , indent 2 "If you are not using the gradle wrapper, ensure gradle executable is in your PATH."
      , ""
      , "Documentation:"
      , indent 2 $ pretty $ "- " <> refGradleDocUrl
      ]
