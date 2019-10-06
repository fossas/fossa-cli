
module App.Scan
  ( scanMain
  ) where

import Prologue

import Control.Concurrent
import Path.IO
import Polysemy
import Polysemy.Async
import Polysemy.Error
import Polysemy.Output
import Polysemy.Resource
import Polysemy.Trace

import Control.Parallel
import Discovery
import Effect.ErrorTrace
import Effect.Exec
import Effect.ReadFS
import Types

scanMain :: Path Abs Dir -> IO ()
scanMain basedir = runFinal
        . embedToFinal @IO
        . resourceToIOFinal
        . asyncToIOFinal
        . execToIO
        . readFSToIO
        . traceToIO
        $ scan basedir

scan :: Members '[Embed IO, Resource, Async, ReadFS, Exec, Trace] r => Path Abs Dir -> Sem r ()
scan basedir = do
  setCurrentDir basedir
  capabilities <- embed $ getNumCapabilities

  runActions capabilities (map ADiscover discoverFuncs) (runAction basedir) updateProgress

runAction :: Members '[Embed IO, Exec, ReadFS, Trace] r => Path Abs Dir -> (Action -> Sem r ()) -> Action -> Sem r ()
runAction basedir enqueue = \case
  ADiscover Discover{..} -> do
    trace $ "Starting discovery: " <> discoverName
    discoverFunc basedir & ignoreErrs & runOutputSem @ConfiguredStrategy (enqueue . AStrategy)
    trace $ "Finished discovery: " <> discoverName

  AStrategy (ConfiguredStrategy Strategy{..} opts)-> do
    trace $ "Starting analysis: " <> strategyName <> " " <> show (strategyModule opts)
    ignoreErrs $ strategyAnalyze opts >>= trace . show
    trace $ "Finished analysis: " <> strategyName <> " " <> show (strategyModule opts)

-- TODO: diagnostics/warning tracing
ignoreErrs :: Sem (Error CLIErr ': r) () -> Sem r ()
ignoreErrs act = either (const ()) id <$> runError act

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
