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
import Effect.ErrorTrace
import Effect.ReadFS

type DiscoverMems r = Members '[Embed IO, Error CLIErr, Output ConfiguredStrategy, ReadFS] r

discoverFuncs :: DiscoverMems r => [Path Abs Dir -> Sem r ()]
discoverFuncs = [Npm.discover, loadConfig strategiesByName]

discovery :: Members '[Embed IO, ErrorTrace, Output ConfiguredStrategy, ReadFS] r => Sem r ()
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


filterFilename :: String -> [Path b File] -> [Path b File]
filterFilename name = filter ((== name) . toFilePath . filename)

filterDirname :: String -> [Path b Dir] -> [Path b Dir]
filterDirname name = filter ((== name) . toFilePath . dirname)
