module App.Fossa.Analyze.Discover (
  discoverFuncs,
  DiscoverFunc (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject, DiscoverTaskEffs)
import Control.Effect.Reader (Has, Reader)
import Data.Aeson qualified as Aeson
import Discovery.Filters (AllFilters)
import Path (Abs, Dir, Path)
import Strategy.Bundler qualified as Bundler
import Strategy.Cargo qualified as Cargo
import Strategy.Carthage qualified as Carthage
import Strategy.Cocoapods qualified as Cocoapods
import Strategy.Composer qualified as Composer
import Strategy.Conda qualified as Conda
import Strategy.Fpm qualified as Fpm
import Strategy.Glide qualified as Glide
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
import Strategy.Python.Pipenv qualified as Pipenv
import Strategy.Python.PyProject qualified as PyProject
import Strategy.Python.Setuptools qualified as Setuptools
import Strategy.R qualified as R
import Strategy.RPM qualified as RPM
import Strategy.Rebar3 qualified as Rebar3
import Strategy.Scala qualified as Scala
import Strategy.SwiftPM qualified as SwiftPM
import Types (DiscoveredProject)

discoverFuncs :: DiscoverTaskEffs sig m => [DiscoverFunc m]
discoverFuncs =
  [ DiscoverFunc Bundler.discover
  , DiscoverFunc Cabal.discover
  , DiscoverFunc Cargo.discover
  , DiscoverFunc Carthage.discover
  , DiscoverFunc Cocoapods.discover
  , DiscoverFunc Composer.discover
  , DiscoverFunc Conda.discover
  , DiscoverFunc Fpm.discover
  , DiscoverFunc Glide.discover
  , DiscoverFunc Godep.discover
  , DiscoverFunc Gomodules.discover
  , DiscoverFunc Gradle.discover
  , DiscoverFunc Leiningen.discover
  , DiscoverFunc Maven.discover
  , DiscoverFunc Mix.discover
  , DiscoverFunc Nim.discover
  , DiscoverFunc Node.discover
  , DiscoverFunc NuGet.discover
  , DiscoverFunc Nuspec.discover
  , DiscoverFunc PackagesConfig.discover
  , DiscoverFunc Paket.discover
  , DiscoverFunc Perl.discover
  , DiscoverFunc Pipenv.discover
  , DiscoverFunc PyProject.discover
  , DiscoverFunc ProjectJson.discover
  , DiscoverFunc Pub.discover
  , DiscoverFunc R.discover
  , DiscoverFunc RPM.discover
  , DiscoverFunc Rebar3.discover
  , DiscoverFunc RepoManifest.discover
  , DiscoverFunc Scala.discover
  , DiscoverFunc Setuptools.discover
  , DiscoverFunc Stack.discover
  , DiscoverFunc SwiftPM.discover
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
  DiscoverFunc :: (AnalyzeProject a, Aeson.ToJSON a, Has (Reader AllFilters) sig m) => (Path Abs Dir -> m [DiscoveredProject a]) -> DiscoverFunc m
