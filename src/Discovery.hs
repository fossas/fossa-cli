module Discovery
  ( discoverFuncs
  , strategyGroups
  ) where

import qualified Strategy.Go.GoList as GoList
import qualified Strategy.Go.Gomod as Gomod
import qualified Strategy.Go.GopkgLock as GopkgLock
import qualified Strategy.Go.GopkgToml as GopkgToml
import qualified Strategy.Go.GlideLock as GlideLock
import qualified Strategy.Gradle as Gradle
import qualified Strategy.Maven as Maven
import qualified Strategy.NpmList as NpmList
import qualified Strategy.Node.NpmLock as NpmLock
import qualified Strategy.Node.PackageJson as PackageJson
import qualified Strategy.Node.YarnLock as YarnLock
import qualified Strategy.NuGet.PackagesConfig as PackagesConfig
import qualified Strategy.NuGet.PackageReference as PackageReference
import qualified Strategy.NuGet.ProjectAssetsJson as ProjectAssetsJson
import qualified Strategy.NuGet.ProjectJson as ProjectJson
import qualified Strategy.NuGet.Nuspec as Nuspec
import qualified Strategy.Python.Pipenv as Pipenv
import qualified Strategy.Python.PipList as PipList
import qualified Strategy.Python.ReqTxt as ReqTxt
import qualified Strategy.Python.SetupPy as SetupPy
import qualified Strategy.Ruby.BundleShow as BundleShow
import qualified Strategy.Ruby.GemfileLock as GemfileLock

import qualified Discovery.Config as Config
import           Types

discoverFuncs :: [Discover]

discoverFuncs =
  [ GoList.discover
  , Gomod.discover
  , GopkgToml.discover
  , GopkgLock.discover
  , GlideLock.discover

  , Gradle.discover

  , Maven.discover

  , PackageJson.discover
  , NpmLock.discover
  , NpmList.discover
  , YarnLock.discover

  , PackagesConfig.discover
  , PackageReference.discover
  , ProjectAssetsJson.discover
  , ProjectJson.discover
  , Nuspec.discover

  , PipList.discover
  , Pipenv.discover
  , SetupPy.discover
  , ReqTxt.discover

  , BundleShow.discover
  , GemfileLock.discover

  , Config.loadConfig strategyGroups
  ]

strategyGroups :: [StrategyGroup]
strategyGroups =
  [ StrategyGroup "dotnet"
      [ SomeStrategy PackagesConfig.strategy
      , SomeStrategy PackageReference.strategy
      , SomeStrategy ProjectAssetsJson.strategy
      , SomeStrategy ProjectJson.strategy
      , SomeStrategy Nuspec.strategy
      ]
  , StrategyGroup "gradle"
      [ SomeStrategy Gradle.strategy
      ]
  , StrategyGroup "maven"
      [ SomeStrategy Maven.strategy
      ]
  , StrategyGroup "nodejs"
      [ SomeStrategy YarnLock.strategy
      , SomeStrategy NpmLock.strategy
      , SomeStrategy NpmList.strategy
      , SomeStrategy PackageJson.strategy
      ]
  , StrategyGroup "python"
      [ SomeStrategy Pipenv.strategyWithCmd
      , SomeStrategy Pipenv.strategyNoCmd
      , SomeStrategy ReqTxt.strategy
      , SomeStrategy SetupPy.strategy
      , SomeStrategy PipList.strategy
      ]
  , StrategyGroup "ruby"
      [ SomeStrategy BundleShow.strategy
      , SomeStrategy GemfileLock.strategy
      ]
  , StrategyGroup "golang"
      [ SomeStrategy GoList.strategy
      , SomeStrategy Gomod.strategy
      , SomeStrategy GopkgLock.strategy
      , SomeStrategy GopkgToml.strategy
      , SomeStrategy GlideLock.strategy
      ]
  ]
