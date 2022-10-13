module Strategy.Scala.Errors (
  MaybeWithoutDependencyTreeTask (..),
  FailedToListProjects (..),
  MissingFullDependencyPlugin (..),

  -- * docs
  scalaFossaDocUrl,
  sbtDepsGraphPluginUrl,
) where

import App.Docs (strategyLangDocUrl)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic, renderDiagnostic)
import Path (Abs, Dir, Path)
import Prettyprinter (Pretty (pretty), indent, viaShow, vsep)

scalaFossaDocUrl :: Text
scalaFossaDocUrl = strategyLangDocUrl "scala/sbt.md"

sbtDepsGraphPluginUrl :: Text
sbtDepsGraphPluginUrl = "https://github.com/sbt/sbt-dependency-graph"

data MaybeWithoutDependencyTreeTask = MaybeWithoutDependencyTreeTask

instance ToDiagnostic MaybeWithoutDependencyTreeTask where
  renderDiagnostic (MaybeWithoutDependencyTreeTask) =
    vsep
      [ "We could not perform dynamic sbt analysis via sbt dependencyTree"
      , indent 2 $
          vsep
            [ "Ensure you can run sbt dependencyTree. If you are using older version than sbt v1.4.0 (e.g. v1.3.13)"
            , "please install following plugin prior to running fossa:"
            , indent 2 $ pretty sbtDepsGraphPluginUrl
            , ""
            ]
      , ""
      , "Refer to:"
      , indent 2 $ pretty $ "- " <> scalaFossaDocUrl
      ]

data MissingFullDependencyPlugin = MissingFullDependencyPlugin

instance ToDiagnostic MissingFullDependencyPlugin where
  renderDiagnostic (MissingFullDependencyPlugin) =
    vsep
      [ "We could not perform dynamic sbt analysis via `sbt dependencyBrowseTreeHTML`"
      , indent 2 $
          vsep
            [ "Ensure you can run `sbt dependencyBrowseTreeHTML`."
            , "Please install following plugin prior to running fossa:"
            , indent 2 $ pretty sbtDepsGraphPluginUrl
            ]
      , ""
      , "Refer to:"
      , indent 2 $ pretty $ "- " <> scalaFossaDocUrl
      ]

newtype FailedToListProjects = FailedToListProjects (Path Abs Dir)
  deriving (Eq, Ord, Show)

instance ToDiagnostic FailedToListProjects where
  renderDiagnostic (FailedToListProjects dir) = "Failed to discover and analyze sbt projects, for sbt build manifest at:" <> viaShow dir
