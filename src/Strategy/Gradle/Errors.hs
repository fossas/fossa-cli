module Strategy.Gradle.Errors (
  FailedToListProjects (..),
  GradleWrapperFailed (..),
  FailedToRunGradleAnalysis (..),
  FailedGradleHelp (..),
  refGradleDocUrl,
) where

import App.Docs (strategyLangDocUrl)

import Data.String.Conversion (toText)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Errata (Errata (..))
import Path (Abs, Dir, Path)

refGradleDocUrl :: Text
refGradleDocUrl = strategyLangDocUrl "gradle/gradle.md"

newtype FailedToListProjects = FailedToListProjects (Path Abs Dir) deriving (Eq, Ord, Show)
instance ToDiagnostic FailedToListProjects where
  renderDiagnostic (FailedToListProjects dir) = do
    let header = "Found a gradle build manifest in " <> toText dir <> " but, could not list projects"
    Errata (Just header) [] Nothing

data FailedGradleHelp = FailedGradleHelp
instance ToDiagnostic FailedGradleHelp where
  renderDiagnostic FailedGradleHelp = do
    let header = "Ensure you can run of the following: `gradlew projects`, `gradlew.bat projects`, `gradle projects`"
    Errata (Just header) [] Nothing

data FailedToRunGradleAnalysis = FailedToRunGradleAnalysis deriving (Eq, Ord, Show)
instance ToDiagnostic FailedToRunGradleAnalysis where
  renderDiagnostic (FailedToRunGradleAnalysis) = do
    let header = "Failed to perform gradle analysis."
    Errata (Just header) [] Nothing

data GradleWrapperFailed = GradleWrapperFailed deriving (Eq, Ord, Show)
instance ToDiagnostic GradleWrapperFailed where
  renderDiagnostic (GradleWrapperFailed) = do
    let header = "Failed to use gradle wrapper, analysis may be inaccurate if gradle executable version differs from expected gradle version."
    Errata (Just header) [] Nothing
