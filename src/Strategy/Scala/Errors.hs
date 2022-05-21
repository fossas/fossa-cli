module Strategy.Scala.Errors (
  MaybeWithoutDependencyTreeTask (..),
  FailedToListProjects (..),

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

newtype MaybeWithoutDependencyTreeTask = MaybeWithoutDependencyTreeTask (Text)

instance ToDiagnostic MaybeWithoutDependencyTreeTask where
  renderDiagnostic (MaybeWithoutDependencyTreeTask project) =
    vsep
      [ "We could not perform dynamic sbt analysis for: " <> viaShow project
      , ""
      , indent 2 $
          vsep
            [ "Ensure you can run sbt dependencyTree. If you are using sbt v1.4.0 or older"
            , "please install following plugin prior to running fossa:"
            , indent 2 $ pretty sbtDepsGraphPluginUrl
            , ""
            ]
      , ""
      , "Refer to:"
      , indent 2 $ pretty $ "- " <> scalaFossaDocUrl
      ]

newtype FailedToListProjects = FailedToListProjects (Path Abs Dir)
  deriving (Eq, Ord, Show)

instance ToDiagnostic FailedToListProjects where
  renderDiagnostic (FailedToListProjects dir) = "Failed to discover and analyze sbt projects, for sbt build manifest at:" <> viaShow dir
