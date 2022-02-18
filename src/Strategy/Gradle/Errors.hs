module Strategy.Gradle.Errors (
  FailedToListProjects (..),
  GradleWrapperFailed (..),
  FailedToRunGradleAnalysis (..),
  refGradleDocUrl,
) where

import App.Docs (strategyLangDocUrl)
import App.Support (reportDefectWithDebugBundle)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Effect.Logger (viaShow)
import Path (Abs, Dir, Path)
import Prettyprinter (indent, vsep)

refGradleDocUrl :: Text
refGradleDocUrl = strategyLangDocUrl "gradle/gradle.md"

newtype FailedToListProjects = FailedToListProjects (Path Abs Dir) deriving (Eq, Ord, Show)
instance ToDiagnostic FailedToListProjects where
  renderDiagnostic (FailedToListProjects dir) =
    vsep
      [ "Found a gradle build manifest in " <> viaShow dir <> " but, could not list projects."
      , ""
      , "Ensure you can run one of:"
      , ""
      , indent 2 "gradlew projects"
      , indent 2 "gradlew.bat projects"
      , indent 2 "gradle projects"
      ]

data FailedToRunGradleAnalysis = FailedToRunGradleAnalysis deriving (Eq, Ord, Show)
instance ToDiagnostic FailedToRunGradleAnalysis where
  renderDiagnostic (FailedToRunGradleAnalysis) =
    vsep
      [ "Failed to perform gradle analysis."
      , ""
      , "Ensure your gradle project can be built successfully:"
      , ""
      , indent 2 "gradlew build"
      , indent 2 "gradlew.bat build"
      , indent 2 "gradle build"
      , ""
      , reportDefectWithDebugBundle
      ]

data GradleWrapperFailed = GradleWrapperFailed deriving (Eq, Ord, Show)
instance ToDiagnostic GradleWrapperFailed where
  renderDiagnostic (GradleWrapperFailed) =
    "Failed to use gradle wrapper, analysis may be inaccurate if gradle executable version differs from expected gradle version."
