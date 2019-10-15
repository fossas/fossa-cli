
module App.Scan
  ( scanMain
  ) where

import Prologue

import Control.Concurrent
import qualified Data.Sequence as S
import Path.IO
import Polysemy
import Polysemy.Async
import Polysemy.Error
import Polysemy.Output
import Polysemy.Resource
import Polysemy.Trace

import App.Scan.Project (mkProjects)
import Control.Parallel
import Diagnostics
import Discovery
import Effect.Exec
import Effect.ReadFS
import Types

scanMain :: Path Abs Dir -> IO ()
scanMain basedir = runFinal
        . embedToFinal @IO
        . resourceToIOFinal
        . asyncToIOFinal
        . traceToIO
        $ scan basedir

scan :: Members '[Final IO, Embed IO, Resource, Async, Trace] r => Path Abs Dir -> Sem r ()
scan basedir = do
  setCurrentDir basedir
  capabilities <- embed getNumCapabilities

  (results, ()) <- runActions capabilities (map ADiscover discoverFuncs) (runAction basedir) updateProgress
    & outputToIOMonoid (S.singleton)

  let projects = mkProjects strategyGroups results
  embed (encodeFile "analysis.json" projects)

runAction :: Members '[Final IO, Embed IO, Trace, Output CompletedStrategy] r => Path Abs Dir -> (Action -> Sem r ()) -> Action -> Sem r ()
runAction basedir enqueue = \case
  ADiscover Discover{..} -> do
    trace $ "Starting discovery: " <> discoverName
    result <- discoverFunc basedir
      & readFSToIO
      & execToIO
      & errorToIOFinal @CLIErr
      & runOutputSem @ConfiguredStrategy (enqueue . AStrategy)

    case result of
      Right () -> trace $ "Finished discovery: " <> discoverName
      Left err -> trace $ "ERROR in discovery: " <> discoverName <> " " <> show err

    trace ""

  AStrategy (ConfiguredStrategy Strategy{..} opts) -> do
    result <- strategyAnalyze opts
      & readFSToIO
      & execToIO
      & errorToIOFinal @CLIErr

    case result of
      Right graph -> do
        trace $ "Finished analysis: " <> strategyName <> " " <> show (strategyModule opts) <> ": " <> show graph
        output (CompletedStrategy strategyName (strategyModule opts) graph strategyOptimal strategyComplete)
      Left err -> trace $ "ERROR in strategy: " <> strategyName <> " " <> show (strategyModule opts) <> ": " <> show err

    trace ""

updateProgress :: Member Trace r => Progress -> Sem r ()
updateProgress Progress{..} =
  trace ( "Queued: "
        <> show pQueued
        <> ", Running: "
        <> show pRunning
        <> ", Completed: "
        <> show pCompleted)

data Action =
    ADiscover Discover
  | AStrategy ConfiguredStrategy
