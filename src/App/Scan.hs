
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

import App.Scan.Project (mkProjects)
import Control.Parallel
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Diagnostics
import Discovery
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Types

scanMain :: Path Abs Dir -> IO ()
scanMain basedir = runFinal
        . embedToFinal @IO
        . resourceToIOFinal
        . asyncToIOFinal
        . loggerToIO Info
        $ scan basedir

scan :: Members '[Final IO, Embed IO, Resource, Async, Logger] r => Path Abs Dir -> Sem r ()
scan basedir = do
  setCurrentDir basedir
  capabilities <- embed getNumCapabilities

  (results, ()) <- runActions capabilities (map ADiscover discoverFuncs) (runAction basedir) updateProgress
    & outputToIOMonoid (S.singleton)

  let projects = mkProjects strategyGroups results
  embed (encodeFile "analysis.json" projects)

runAction :: Members '[Final IO, Embed IO, Logger, Output CompletedStrategy] r => Path Abs Dir -> (Action -> Sem r ()) -> Action -> Sem r ()
runAction basedir enqueue = \case
  ADiscover Discover{..} -> do
    let prettyName = fill 20 (annotate (colorDull Cyan) (pretty discoverName <> " "))

    result <- discoverFunc basedir
      & readFSToIO
      & execToIO
      & errorToIOFinal @CLIErr
      & runOutputSem @ConfiguredStrategy (enqueue . AStrategy)

    case result of
      Right () -> logDebug $ prettyName <> annotate (color Green) "Finished discovery"
      Left err -> do
        logWarn $ prettyName <> annotate (color Red) "Discovery failed"
        logDebug $ pretty (show err) <> line

  AStrategy (ConfiguredStrategy Strategy{..} opts) -> do
    let prettyName = fill 20 (annotate (color Cyan) (pretty strategyName <> " "))
        prettyPath = pretty (toFilePath (strategyModule opts))

    result <- strategyAnalyze opts
      & readFSToIO
      & execToIO
      & errorToIOFinal @CLIErr

    case result of
      Right graph -> do
        logInfo $ prettyName <> prettyPath <> " " <> annotate (color Green) "Analyzed"
        logDebug (pretty (show graph))
        output (CompletedStrategy strategyName (strategyModule opts) graph strategyOptimal strategyComplete)
      Left err -> do
        logWarn $ prettyName <> prettyPath <> " " <> annotate (color Yellow) "Analysis failed"
        logDebug $ pretty (show err) <> line

updateProgress :: Member Logger r => Progress -> Sem r ()
updateProgress Progress{..} =
  logSticky ( "[ "
            <> annotate (color Cyan) (pretty pQueued)
            <> " Waiting / "
            <> annotate (color Yellow) (pretty pRunning)
            <> " Running / "
            <> annotate (color Green) (pretty pCompleted)
            <> " Completed"
            <> " ]" )

data Action =
    ADiscover Discover
  | AStrategy ConfiguredStrategy
