module Strategy.Scala.Errors (
  MaybeWithoutDependencyTreeTask (..),
  FailedToListProjects (..),
  MissingFullDependencyPlugin (..),

  -- * docs
  scalaFossaDocUrl,
  sbtDepsGraphPluginUrl,
) where

import App.Docs (strategyLangDocUrl)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic, renderDiagnostic)
import Errata (Errata (..))
import Path (Abs, Dir, Path)

scalaFossaDocUrl :: Text
scalaFossaDocUrl = strategyLangDocUrl "scala/sbt.md"

sbtDepsGraphPluginUrl :: Text
sbtDepsGraphPluginUrl = "https://github.com/sbt/sbt-dependency-graph"

data MaybeWithoutDependencyTreeTask
  = MaybeWithoutDependencyTreeTaskCtx
  | MaybeWithoutDependencyTreeTaskHelp

instance ToDiagnostic MaybeWithoutDependencyTreeTask where
  renderDiagnostic MaybeWithoutDependencyTreeTaskCtx = do
    let header = "Could not perform dynamic sbt analysis via sbt dependencyTree"
    Errata (Just header) [] Nothing
  renderDiagnostic MaybeWithoutDependencyTreeTaskHelp = do
    let header = "Ensure you can run sbt dependencyTree. Install the sbt plugin if you are using a version older than sbt v1.4.0 (e.g. v1.3.13)"
    Errata (Just header) [] Nothing

data MissingFullDependencyPlugin
  = MissingFullDependencyPluginCtx
  | MissingFullDependencyPluginHelp

instance ToDiagnostic MissingFullDependencyPlugin where
  renderDiagnostic MissingFullDependencyPluginCtx = do
    let header = "Could not perform dynamic sbt analysis via `sbt dependencyBrowseTreeHTML`"
    Errata (Just header) [] Nothing
  renderDiagnostic MissingFullDependencyPluginHelp = do
    let header = "Ensure you can run `sbt dependencyBrowseTreeHTML`. Install the sbt plugin if you are not able to run the command."
    Errata (Just header) [] Nothing

newtype FailedToListProjects = FailedToListProjects (Path Abs Dir)
  deriving (Eq, Ord, Show)

instance ToDiagnostic FailedToListProjects where
  renderDiagnostic (FailedToListProjects dir) = do
    let header = "Failed to discover and analyze sbt projects, for sbt build manifest at: " <> toText dir
    Errata (Just header) [] Nothing
