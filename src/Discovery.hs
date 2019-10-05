module Discovery
  ( discoverFuncs
  , strategyGroups
  ) where

import qualified Strategy.Npm as Npm
import qualified Strategy.Python.Pipenv as Pipenv
import qualified Strategy.Python.PipList as PipList
import qualified Strategy.Python.ReqTxt as ReqTxt
import qualified Strategy.Python.SetupPy as SetupPy

import qualified Discovery.Config as Config
import           Types

discoverFuncs :: [Discover]
discoverFuncs = [Npm.discover, PipList.discover, Pipenv.discover, SetupPy.discover, ReqTxt.discover, Config.loadConfig strategyGroups]

strategyGroups :: [StrategyGroup]
strategyGroups =
  [ StrategyGroup "nodejs"
      [ SomeStrategy Npm.strategy
      ]
  , StrategyGroup "python"
      [ SomeStrategy PipList.strategy
      , SomeStrategy Pipenv.strategy
      , SomeStrategy SetupPy.strategy
      , SomeStrategy ReqTxt.strategy
      ]
  ]
