module App.Fossa.Container.Sources.Discovery (
  layerAnalyzers,
  renderLayerTarget,
) where

import App.Fossa.Analyze.Discover (DiscoverFunc (DiscoverFunc))
import App.Fossa.Analyze.Types (
  AnalyzeStaticTaskEffs,
 )
import Container.OsRelease (OsInfo)
import Control.Effect.Stack (Has)
import Data.Foldable (for_)
import Data.Set qualified as Set
import Data.Set.NonEmpty (toSet)
import Data.Text (Text)
import Effect.Logger (
  AnsiStyle,
  Doc,
  Logger,
  Pretty (pretty),
  logInfo,
 )
import Path (Abs, Dir, Path, Rel, toFilePath)
import Path.IO (RelPath, makeRelative)
import Strategy.ApkDatabase qualified as Apk
import Strategy.BerkeleyDB qualified as BerkeleyDB
import Strategy.Bundler qualified as Bundler
import Strategy.Carthage qualified as Carthage
import Strategy.Cocoapods qualified as Cocoapods
import Strategy.Composer qualified as Composer
import Strategy.Dpkg qualified as Dpkg
import Strategy.Fpm qualified as Fpm
import Strategy.Glide qualified as Glide
import Strategy.Googlesource.RepoManifest qualified as RepoManifest
import Strategy.Maven qualified as Maven
import Strategy.NDB qualified as NDB
import Strategy.Nim qualified as Nim
import Strategy.Node qualified as Node
import Strategy.NuGet.Nuspec qualified as Nuspec
import Strategy.NuGet.PackageReference qualified as PackageReference
import Strategy.NuGet.PackagesConfig qualified as PackagesConfig
import Strategy.NuGet.Paket qualified as Paket
import Strategy.NuGet.ProjectAssetsJson qualified as ProjectAssetsJson
import Strategy.NuGet.ProjectJson qualified as ProjectJson
import Strategy.Pdm qualified as Pdm
import Strategy.Perl qualified as Perl
import Strategy.Pub qualified as Pub
import Strategy.Python.Pipenv qualified as Pipenv
import Strategy.Python.Poetry qualified as Poetry
import Strategy.Python.Setuptools qualified as Setuptools
import Strategy.R qualified as R
import Strategy.RPM qualified as RPM
import Strategy.Sqlite qualified as Sqlite
import Strategy.SwiftPM qualified as SwiftPM
import Types (
  BuildTarget (unBuildTarget),
  DiscoveredProject (projectBuildTargets, projectPath, projectType),
  DiscoveredProjectType,
  FoundTargets (FoundTargets, ProjectWithoutTargets),
 )

layerAnalyzers :: AnalyzeStaticTaskEffs sig m => OsInfo -> Bool -> [DiscoverFunc m]
layerAnalyzers os onlySystemDeps =
  if onlySystemDeps
    then osDepsAnalyzers os
    else osDepsAnalyzers os ++ managedDepsDiscoveryF

osDepsAnalyzers :: AnalyzeStaticTaskEffs sig m => OsInfo -> [DiscoverFunc m]
osDepsAnalyzers osInfo =
  [ DiscoverFunc (Apk.discover osInfo)
  , DiscoverFunc (Dpkg.discover osInfo)
  , DiscoverFunc (BerkeleyDB.discover osInfo)
  , DiscoverFunc (Sqlite.discover osInfo)
  , DiscoverFunc (NDB.discover osInfo)
  ]

managedDepsDiscoveryF :: AnalyzeStaticTaskEffs sig m => [DiscoverFunc m]
managedDepsDiscoveryF =
  [ DiscoverFunc Bundler.discover
  , DiscoverFunc Carthage.discover
  , DiscoverFunc Cocoapods.discover
  , DiscoverFunc Composer.discover
  , DiscoverFunc Fpm.discover
  , DiscoverFunc Glide.discover
  , DiscoverFunc Maven.discover
  , DiscoverFunc Nim.discover
  , DiscoverFunc Node.discover
  , DiscoverFunc Nuspec.discover
  , DiscoverFunc PackageReference.discover
  , DiscoverFunc PackagesConfig.discover
  , DiscoverFunc Paket.discover
  , DiscoverFunc Pdm.discover
  , DiscoverFunc Perl.discover
  , DiscoverFunc Pipenv.discover
  , DiscoverFunc Poetry.discover
  , DiscoverFunc ProjectAssetsJson.discover
  , DiscoverFunc ProjectJson.discover
  , DiscoverFunc Pub.discover
  , DiscoverFunc R.discover
  , DiscoverFunc RPM.discover
  , DiscoverFunc RepoManifest.discover
  , DiscoverFunc Setuptools.discover
  , DiscoverFunc SwiftPM.discover
  --
  -- Following can be performed only with dynamic analysis.
  -- So we don not do any discovery for them (to avoid error noise)
  --
  -- , DiscoverFunc Cabal.discover
  -- , DiscoverFunc Cargo.discover
  -- , DiscoverFunc Conda.discover
  -- , DiscoverFunc Godep.discover
  -- , DiscoverFunc Gomodules.discover
  -- , DiscoverFunc Gradle.discover
  -- , DiscoverFunc Leiningen.discover
  -- , DiscoverFunc Mix.discover
  -- , DiscoverFunc Rebar3.discover
  -- , DiscoverFunc Scala.discover
  -- , DiscoverFunc Stack.discover
  ]

-- | Logs a discovered project, with it's project type, path, targets, and layer in info severity.
renderLayerTarget :: Has Logger sig m => Path Abs Dir -> Text -> DiscoveredProject a -> m ()
renderLayerTarget basedir layer project =
  case relDir of
    Nothing -> pure ()
    Just relPath -> do
      let typeOfProject = projectType project
      logInfo $ "Found project: " <> locationOf typeOfProject relPath Nothing

      case projectBuildTargets project of
        ProjectWithoutTargets -> do
          logInfo $ "Found target: " <> locationOf typeOfProject relPath Nothing
        FoundTargets targets -> for_ (Set.toList $ toSet targets) $ \target -> do
          logInfo $ "Found target: " <> locationOf typeOfProject relPath (Just target)
  where
    relDir :: Maybe (RelPath (Path Abs Dir))
    relDir = makeRelative basedir (projectPath project)

    locationOf :: DiscoveredProjectType -> Path Rel Dir -> Maybe BuildTarget -> Doc AnsiStyle
    locationOf typeOfProject rel buildTarget =
      pretty typeOfProject
        <> "@"
        <> pretty (toFilePath rel)
        <> maybe mempty (pretty . unBuildTarget) buildTarget
        <> pretty (" (" <> layer <> ")")
