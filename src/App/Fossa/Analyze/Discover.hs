module App.Fossa.Analyze.Discover (
  discoverFuncs,
  DiscoverFunc (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject, DiscoverTaskEffs)
import Control.Effect.Reader (Has, Reader)
import Data.Aeson qualified as Aeson
import Discovery.Filters (AllFilters)
import Discovery.Simple (cachedDiscover, cachedDiscoverWith)
import Discovery.Walk (WalkStep)
import Path (Abs, Dir, File, Path)
import Strategy.Bundler qualified as Bundler
import Strategy.Cargo qualified as Cargo
import Strategy.Carthage qualified as Carthage
import Strategy.Cocoapods qualified as Cocoapods
import Strategy.Composer qualified as Composer
import Strategy.Conan qualified as Conan
import Strategy.Conda qualified as Conda
import Strategy.Fpm qualified as Fpm
import Strategy.Glide qualified as Glide
import Strategy.Go.GoBinary qualified as GoBinary
import Strategy.Godep qualified as Godep
import Strategy.Gomodules qualified as Gomodules
import Strategy.Googlesource.RepoManifest qualified as RepoManifest
import Strategy.Gradle qualified as Gradle
import Strategy.Haskell.Cabal qualified as Cabal
import Strategy.Haskell.Stack qualified as Stack
import Strategy.Leiningen qualified as Leiningen
import Strategy.Maven qualified as Maven
import Strategy.Mix qualified as Mix
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
import Strategy.Rebar3 qualified as Rebar3
import Strategy.Scala qualified as Scala
import Strategy.SwiftPM qualified as SwiftPM
import Types (DiscoveredProject, DiscoveredProjectType (..))

discoverFuncs :: DiscoverTaskEffs sig m => [DiscoverFunc m]
discoverFuncs =
  [ WalkedFunc
      Bundler.discoverVisitor
      (cachedDiscover BundlerProjectType Bundler.mkProject)
  , WalkedFunc
      Cabal.discoverVisitor
      (cachedDiscover CabalProjectType Cabal.mkProject)
  , WalkedFunc
      Cargo.discoverVisitor
      (cachedDiscover CargoProjectType Cargo.mkProject)
  , WalkedFunc
      Carthage.discoverVisitor
      (cachedDiscover CarthageProjectType Carthage.mkProject)
  , WalkedFunc
      Cocoapods.discoverVisitor
      (cachedDiscover CocoapodsProjectType Cocoapods.mkProject)
  , WalkedFunc
      Composer.discoverVisitor
      (cachedDiscover ComposerProjectType Composer.mkProject)
  , WalkedFunc
      Conan.discoverVisitor
      (cachedDiscover ConanProjectType Conan.mkProject)
  , WalkedFunc
      Conda.discoverVisitor
      (cachedDiscover CondaProjectType Conda.mkProject)
  , WalkedFunc
      Fpm.discoverVisitor
      (cachedDiscover FpmProjectType Fpm.mkProject)
  , WalkedFunc
      Glide.discoverVisitor
      (cachedDiscover GlideProjectType Glide.mkProject)
  , WalkedFunc
      Godep.discoverVisitor
      (cachedDiscover GodepProjectType Godep.mkProject)
  , DiscoverFunc GoBinary.discover
  , WalkedFunc
      Gomodules.discoverVisitor
      (cachedDiscover GomodProjectType Gomodules.mkProject)
  , WalkedFunc
      Gradle.discoverVisitor
      (cachedDiscover GradleProjectType Gradle.mkProject)
  , WalkedFunc
      Leiningen.discoverVisitor
      (cachedDiscover LeiningenProjectType Leiningen.mkProject)
  , DiscoverFunc Maven.discover
  , WalkedFunc
      Mix.discoverVisitor
      (cachedDiscover MixProjectType Mix.mkProject)
  , WalkedFunc
      Nim.discoverVisitor
      (cachedDiscover NimbleProjectType Nim.mkProject)
  , DiscoverFunc Node.discover
  , WalkedFunc
      NuGet.discoverVisitor
      (cachedDiscover NuGetProjectType NuGet.mkProject)
  , WalkedFunc
      Nuspec.discoverVisitor
      (cachedDiscover NuspecProjectType Nuspec.mkProject)
  , WalkedFunc
      PackagesConfig.discoverVisitor
      (cachedDiscover PackagesConfigProjectType PackagesConfig.mkProject)
  , WalkedFunc
      Paket.discoverVisitor
      (cachedDiscover PaketProjectType Paket.mkProject)
  , WalkedFunc
      Pdm.discoverVisitor
      (cachedDiscover PdmProjectType Pdm.mkProject)
  , WalkedFunc
      Perl.discoverVisitor
      (cachedDiscover PerlProjectType Perl.mkProject)
  , WalkedFunc
      Pipenv.discoverVisitor
      (cachedDiscover PipenvProjectType Pipenv.mkProject)
  , WalkedFunc
      Poetry.discoverVisitor
      (cachedDiscover PoetryProjectType Poetry.mkProject)
  , WalkedFunc
      ProjectJson.discoverVisitor
      (cachedDiscover ProjectJsonProjectType ProjectJson.mkProject)
  , WalkedFunc
      Pub.discoverVisitor
      (cachedDiscover PubProjectType Pub.mkProject)
  , WalkedFunc
      R.discoverVisitor
      (cachedDiscover RProjectType R.mkProject)
  , WalkedFunc
      RPM.discoverVisitor
      (cachedDiscover RpmProjectType RPM.mkProject)
  , WalkedFunc
      Rebar3.discoverVisitor
      (cachedDiscover Rebar3ProjectType Rebar3.mkProject)
  , WalkedFunc
      RepoManifest.discoverVisitor
      (cachedDiscover RepoManifestProjectType RepoManifest.mkProject)
  , WalkedFunc
      Scala.discoverVisitor
      (cachedDiscoverWith ScalaProjectType Scala.postDiscover)
  , WalkedFunc
      Setuptools.discoverVisitor
      (cachedDiscover SetuptoolsProjectType Setuptools.mkProject)
  , WalkedFunc
      Stack.discoverVisitor
      (cachedDiscover StackProjectType Stack.mkProject)
  , WalkedFunc
      SwiftPM.discoverVisitor
      (cachedDiscover SwiftProjectType SwiftPM.mkProject)
  , WalkedFunc
      Uv.discoverVisitor
      (cachedDiscover PipenvProjectType Uv.mkProject)
  ]

-- DiscoverFunc is a workaround for the lack of impredicative types.
--
-- @discoverFuncs@ is a heterogenous list of discover functions that produce
-- different types of projects we can analyze for dependencies.
--
-- This GADT allows us to say that we don't care about the specific type of
-- projects produced by a discover function; we only care that each project type
-- implements ToJSON and AnalyzeProject
--
-- With impredicative types, we could shift the @forall@ inside the list,
-- avoiding the need for this GADT
--
--     discoverFuncs ::
--       AnalyzeTaskEffs sig m =>
--       [forall a. (AnalyzeProject a, ToJSON a) =>
--          Path Abs Dir -> m [DiscoveredProject a]]
data DiscoverFunc m where
  -- | Strategy that has not (yet) been migrated to the shared walk: it
  -- discovers projects itself, starting from the given directory.
  DiscoverFunc ::
    ( AnalyzeProject a
    , Aeson.ToJSON a
    , Has (Reader AllFilters) sig m
    ) =>
    (Path Abs Dir -> m [DiscoveredProject a]) ->
    DiscoverFunc m
  -- | Strategy whose per-directory visitor is driven over the shared discovery
  -- walk ("Discovery.WalkStream.walkStream" with "Discovery.WalkVisitor.runVisitors"),
  -- so the tree is traversed once for all strategies instead of once per
  -- strategy.  The
  -- finalizer receives an action reading the raw projects the visitor
  -- collected during that walk, and turns them into 'DiscoveredProject's.
  WalkedFunc ::
    ( AnalyzeProject p
    , Aeson.ToJSON p
    ) =>
    -- | visitor, called once per directory by the shared walk
    (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m ([raw], WalkStep)) ->
    -- | finalize the raw projects collected by the visitor
    (m [raw] -> m [DiscoveredProject p]) ->
    DiscoverFunc m
