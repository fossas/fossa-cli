module App.Fossa.Container.Sources.Discovery (
  layerAnalyzers,
  renderLayerTarget,
) where

import App.Fossa.Analyze.Discover (DiscoverFunc (..))
import App.Fossa.Analyze.Types (
  AnalyzeStaticTaskEffs,
 )
import Container.OsRelease (OsInfo)
import Control.Effect.Stack (Has)
import Data.Foldable (for_)
import Data.Set qualified as Set
import Data.Set.NonEmpty (toSet)
import Data.Text (Text)
import Discovery.Simple (cachedDiscover, cachedDiscoverWith)
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
import Strategy.Conan qualified as Conan
import Strategy.Dpkg qualified as Dpkg
import Strategy.Fpm qualified as Fpm
import Strategy.Glide qualified as Glide
import Strategy.Googlesource.RepoManifest qualified as RepoManifest
import Strategy.Maven qualified as Maven
import Strategy.NDB qualified as NDB
import Strategy.Nim qualified as Nim
import Strategy.Node qualified as Node
import Strategy.NuGet qualified as NuGet
import Strategy.NuGet.Nuspec qualified as Nuspec
import Strategy.NuGet.PackagesConfig qualified as PackagesConfig
import Strategy.NuGet.Paket qualified as Paket
import Strategy.NuGet.ProjectJson qualified as ProjectJson
import Strategy.Perl qualified as Perl
import Strategy.Pub qualified as Pub
import Strategy.Python.PDM.Pdm qualified as Pdm
import Strategy.Python.Pipenv qualified as Pipenv
import Strategy.Python.Poetry qualified as Poetry
import Strategy.Python.Setuptools qualified as Setuptools
import Strategy.Python.Uv qualified as Uv
import Strategy.R qualified as R
import Strategy.RPM qualified as RPM
import Strategy.Sqlite qualified as Sqlite
import Strategy.SwiftPM qualified as SwiftPM
import Types (
  BuildTarget (unBuildTarget),
  DiscoveredProject (projectBuildTargets, projectPath, projectType),
  DiscoveredProjectType (..),
  FoundTargets (FoundTargets, ProjectWithoutTargets),
 )

layerAnalyzers :: AnalyzeStaticTaskEffs sig m => Maybe OsInfo -> Bool -> [DiscoverFunc m]
layerAnalyzers os onlySystemDeps = do
  -- For true distroless container support the linux builds need to be able to produce a result without os info.
  -- Not having OS info will mean we miss any system deps in the container.
  -- It's still useful to allow this though because we can do jar in container analysis as well as regular project analysis.
  let systemDeps = maybe [] osDepsAnalyzers os
  if onlySystemDeps
    then systemDeps
    else systemDeps ++ managedDepsDiscoveryF

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
  -- Each entry mirrors the corresponding post-#1821 entry in
  -- 'App.Fossa.Analyze.Discover.discoverFuncs': the per-directory visitor is
  -- driven over the shared walk by 'Discovery.Walk.Broadcast', and the
  -- finalizer turns the collected raw projects into discovered projects.
  [ WalkedFunc
      BundlerProjectType
      Bundler.discoverVisitor
      (cachedDiscover BundlerProjectType Bundler.mkProject)
  , WalkedFunc
      CarthageProjectType
      Carthage.discoverVisitor
      (cachedDiscover CarthageProjectType Carthage.mkProject)
  , WalkedFunc
      CocoapodsProjectType
      Cocoapods.discoverVisitor
      (cachedDiscover CocoapodsProjectType Cocoapods.mkProject)
  , WalkedFunc
      ComposerProjectType
      Composer.discoverVisitor
      (cachedDiscover ComposerProjectType Composer.mkProject)
  , WalkedFunc
      ConanProjectType
      Conan.discoverVisitor
      (cachedDiscover ConanProjectType Conan.mkProject)
  , WalkedFunc
      FpmProjectType
      Fpm.discoverVisitor
      (cachedDiscover FpmProjectType Fpm.mkProject)
  , WalkedFunc
      GlideProjectType
      Glide.discoverVisitor
      (cachedDiscover GlideProjectType Glide.mkProject)
  , WalkedFunc
      MavenProjectType
      Maven.discoverVisitor
      (cachedDiscoverWith MavenProjectType Maven.postDiscover)
  , WalkedFunc
      NimbleProjectType
      Nim.discoverVisitor
      (cachedDiscover NimbleProjectType Nim.mkProject)
  , WalkedFunc
      NpmProjectType
      Node.discoverVisitor
      Node.finalizeProjects
  , WalkedFunc
      NuGetProjectType
      NuGet.discoverVisitor
      (cachedDiscoverWith NuGetProjectType NuGet.postDiscover)
  , WalkedFunc
      NuspecProjectType
      Nuspec.discoverVisitor
      (cachedDiscover NuspecProjectType Nuspec.mkProject)
  , WalkedFunc
      PackagesConfigProjectType
      PackagesConfig.discoverVisitor
      (cachedDiscover PackagesConfigProjectType PackagesConfig.mkProject)
  , WalkedFunc
      PaketProjectType
      Paket.discoverVisitor
      (cachedDiscover PaketProjectType Paket.mkProject)
  , WalkedFunc
      PdmProjectType
      Pdm.discoverVisitor
      (cachedDiscover PdmProjectType Pdm.mkProject)
  , WalkedFunc
      PerlProjectType
      Perl.discoverVisitor
      (cachedDiscover PerlProjectType Perl.mkProject)
  , WalkedFunc
      PipenvProjectType
      Pipenv.discoverVisitor
      (cachedDiscover PipenvProjectType Pipenv.mkProject)
  , WalkedFunc
      PoetryProjectType
      Poetry.discoverVisitor
      (cachedDiscover PoetryProjectType Poetry.mkProject)
  , WalkedFunc
      ProjectJsonProjectType
      ProjectJson.discoverVisitor
      (cachedDiscover ProjectJsonProjectType ProjectJson.mkProject)
  , WalkedFunc
      PubProjectType
      Pub.discoverVisitor
      (cachedDiscover PubProjectType Pub.mkProject)
  , WalkedFunc
      RProjectType
      R.discoverVisitor
      (cachedDiscover RProjectType R.mkProject)
  , WalkedFunc
      RpmProjectType
      RPM.discoverVisitor
      (cachedDiscover RpmProjectType RPM.mkProject)
  , WalkedFunc
      RepoManifestProjectType
      RepoManifest.discoverVisitor
      (cachedDiscover RepoManifestProjectType RepoManifest.mkProject)
  , WalkedFunc
      SetuptoolsProjectType
      Setuptools.discoverVisitor
      (cachedDiscover SetuptoolsProjectType Setuptools.mkProject)
  , WalkedFunc
      SwiftProjectType
      SwiftPM.swiftPackageVisitor
      (cachedDiscover SwiftProjectType SwiftPM.mkProject)
  , WalkedFunc
      SwiftProjectType
      SwiftPM.xcodeProjectVisitor
      (cachedDiscover SwiftProjectType SwiftPM.mkProject)
  , WalkedFunc
      UvProjectType
      Uv.discoverVisitor
      (cachedDiscover UvProjectType Uv.mkProject)
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
