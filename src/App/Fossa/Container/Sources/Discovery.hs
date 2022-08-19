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
import Strategy.Bundler qualified as Bundler
import Strategy.Carthage qualified as Carthage
import Strategy.Composer qualified as Composer
import Strategy.Dpkg qualified as Dpkg
import Strategy.Fpm qualified as Fpm
import Strategy.Glide qualified as Glide
import Strategy.Googlesource.RepoManifest qualified as RepoManifest
import Strategy.Node qualified as Node
import Strategy.NuGet.Nuspec qualified as Nuspec
import Strategy.NuGet.PackageReference qualified as PackageReference
import Strategy.NuGet.PackagesConfig qualified as PackagesConfig
import Strategy.NuGet.Paket qualified as Paket
import Strategy.NuGet.ProjectAssetsJson qualified as ProjectAssetsJson
import Strategy.NuGet.ProjectJson qualified as ProjectJson
import Strategy.Perl qualified as Perl
import Strategy.Python.Poetry qualified as Poetry
import Strategy.Python.Setuptools qualified as Setuptools
import Strategy.RPM qualified as RPM
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
    else osDepsAnalyzers os ++ staticOnlyAnalyzers

osDepsAnalyzers :: AnalyzeStaticTaskEffs sig m => OsInfo -> [DiscoverFunc m]
osDepsAnalyzers osInfo =
  [ DiscoverFunc (Apk.discover osInfo)
  , DiscoverFunc (Dpkg.discover osInfo)
  ]

-- | Static analyzers, not requiring Exec Effect.
--
-- Instead of analyzing _static_ tactics for all analyzers, we intentionally
-- choose analyzer, which support static tactics only. We do this, to avoid
-- scenario in which customer using Container Scanning for Image sees, a
-- conflicting result between scanned container image, and provided build
-- for a project which is persisted in container image.
--
-- If customer requires, suboptimal static tactics, instead of having any
-- analysis, we can make such change as needed in future. This is intentionally
-- kept simple for now.
staticOnlyAnalyzers :: AnalyzeStaticTaskEffs sig m => [DiscoverFunc m]
staticOnlyAnalyzers =
  [ DiscoverFunc Bundler.discover
  , DiscoverFunc Carthage.discover
  , DiscoverFunc Composer.discover
  , DiscoverFunc Fpm.discover
  , DiscoverFunc Glide.discover
  , DiscoverFunc Node.discover
  , DiscoverFunc Nuspec.discover
  , DiscoverFunc PackageReference.discover
  , DiscoverFunc PackagesConfig.discover
  , DiscoverFunc Paket.discover
  , DiscoverFunc Perl.discover
  , DiscoverFunc Poetry.discover
  , DiscoverFunc ProjectAssetsJson.discover
  , DiscoverFunc ProjectJson.discover
  , DiscoverFunc RPM.discover
  , DiscoverFunc RepoManifest.discover
  , DiscoverFunc Setuptools.discover
  , DiscoverFunc SwiftPM.discover
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
