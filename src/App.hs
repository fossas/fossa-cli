
{-# language QuasiQuotes #-} -- TODO: remove

module App
  ( appMain
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

import           Control.Parallel
import           Discovery
import           Effect.ErrorTrace
import           Effect.Exec
import           Effect.ReadFS
import           Types

appMain :: IO ()
appMain = void
        . runFinal
        . embedToFinal @IO
        . resourceToIOFinal
        . asyncToIOFinal
        . execToIO
        . readFSToIO
        . traceToIO $ do

  embed @IO $ setCurrentDir [absdir|/Users/connor/.go/src/github.com/fossas/fossa-cli/|]

  basedir <- getCurrentDir
  capabilities <- embed $ getNumCapabilities

  runActions capabilities (map ADiscover discoverFuncs) (runAction basedir) updateProgress

  where

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
