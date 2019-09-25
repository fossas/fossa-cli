module Discovery
  ( filterFilename
  , filterDirname
  , discovery
  ) where

import Prologue

import qualified Data.Map as M
import           Path.IO
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Strategy
import qualified Strategy.Npm as Npm

import Config
import Effect.ReadFS

discovery :: Members '[Embed IO, Error ConfigErr, Output ConfiguredStrategy, ReadFS] r => Sem r ()
discovery = do
  dir <- getCurrentDir
  void $ sequence [Npm.discover dir, loadConfig strategiesByName dir]

-- TODO: newtypes?
type ModuleType = String
type StrategyName = String

moduleTypes :: Map ModuleType [StrategyName]
moduleTypes = M.fromList
  [ ("nodejs", ["nodejs-npm"])
  ]

strategiesByName :: Map String SomeStrategy
strategiesByName = M.fromList (map (\strategy@(SomeStrategy Strategy{strategyName}) -> (strategyName, strategy)) strategies)

strategies :: [SomeStrategy]
strategies = [SomeStrategy Npm.strategy]


filterFilename :: String -> [Path b File] -> [Path b File]
filterFilename name = filter ((== name) . toFilePath . filename)

filterDirname :: String -> [Path b Dir] -> [Path b Dir]
filterDirname name = filter ((== name) . toFilePath . dirname)
