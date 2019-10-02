module Discovery
  ( discovery
  ) where

import Prologue

import qualified Data.Map as M
import           Path.IO
import           Polysemy
import           Polysemy.Async
import           Polysemy.Error
import           Polysemy.Output
import           Strategy
import qualified Strategy.Npm as Npm
import qualified Strategy.Python.Pipenv as Pipenv
import qualified Strategy.Python.PipList as PipList
import qualified Strategy.Python.ReqTxt as ReqTxt
import qualified Strategy.Python.SetupPy as SetupPy

import Config
import Discovery.Core
import Effect.ErrorTrace
import Effect.ReadFS

discoverFuncs :: DiscoverEffs r => [Path Abs Dir -> Sem r ()]
discoverFuncs = [Npm.discover, PipList.discover, Pipenv.discover, SetupPy.discover, ReqTxt.discover, loadConfig strategiesByName]

discovery :: Members '[Embed IO, Async, ErrorTrace, Output ConfiguredStrategy, ReadFS] r => Sem r ()
discovery = do
  dir <- getCurrentDir
  for_ discoverFuncs $ \discover -> do
    result <- runError @CLIErr $ (discover dir)
    case result of
      Left err -> traceErr err
      Right _ -> pure ()

strategiesByName :: Map String SomeStrategy
strategiesByName = M.fromList (map (\strategy@(SomeStrategy Strategy{strategyName}) -> (strategyName, strategy)) strategies)

strategies :: [SomeStrategy]
strategies = [SomeStrategy Npm.strategy]
