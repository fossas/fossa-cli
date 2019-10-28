module Discovery
  ( discoverFuncs
  , strategyGroups
  ) where

import qualified Strategy.Gradle as Gradle
import qualified Strategy.NpmList as NpmList
import qualified Strategy.Python.Pipenv as Pipenv
import qualified Strategy.Python.PipList as PipList
import qualified Strategy.Python.ReqTxt as ReqTxt
import qualified Strategy.Python.SetupPy as SetupPy
import qualified Strategy.Ruby.BundleShow as BundleShow

import qualified Discovery.Config as Config
import           Types

discoverFuncs :: [Discover]
discoverFuncs = [Gradle.discover, NpmList.discover, PipList.discover, Pipenv.discover, SetupPy.discover, ReqTxt.discover, BundleShow.discover, Config.loadConfig strategyGroups]

strategyGroups :: [StrategyGroup]
strategyGroups =
  [ StrategyGroup "gradle"
      [ SomeStrategy Gradle.strategy
      ]
  , StrategyGroup "nodejs"
      [ SomeStrategy NpmList.strategy
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
      ]
  ]
