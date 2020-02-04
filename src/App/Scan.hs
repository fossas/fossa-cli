
module App.Scan
  ( scanMain
  ) where

import Prologue

import Control.Concurrent
import Control.Exception (SomeException)
import Data.Bool (bool)
import qualified Data.Sequence as S
import Path.IO
import Polysemy
import Polysemy.Async
import Polysemy.Error
import Polysemy.Output
import Polysemy.Resource
import System.Exit (die)

import App.Scan.Project (mkProjects)
import App.Scan.ProjectInference (InferredProject(..), inferProject)
import Control.Parallel
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Diagnostics
import Discovery
import Effect.Error
import Effect.Exec
import Effect.Logger
import Effect.ReadFS hiding (doesDirExist)
import Types

scanMain :: Path Abs Dir -> Bool -> IO ()
scanMain basedir debug = do
  exists <- doesDirExist basedir
  unless exists (die $ "ERROR: " <> show basedir <> " does not exist")

  scan basedir
    & loggerToIO (bool Info Debug debug)
    & asyncToIOFinal
    & resourceToIOFinal
    & embedToFinal @IO
    & runFinal

scan :: Members '[Final IO, Embed IO, Resource, Async, Logger] r => Path Abs Dir -> Sem r ()
scan basedir = do
  setCurrentDir basedir
  capabilities <- embed getNumCapabilities

  (results, ()) <- runActions capabilities (map ADiscover discoverFuncs) (runAction basedir) updateProgress
    & outputToIOMonoid S.singleton

  logSticky "[ Combining Analyses ]"

  let projects = mkProjects strategyGroups results
  embed (encodeFile "analysis.json" projects)

  inferred <- inferProject basedir
  logInfo ""
  logInfo ("Inferred project name: `" <> pretty (inferredName inferred) <> "`")
  logInfo ("Inferred revision: `" <> pretty (inferredRevision inferred) <> "`")

  logSticky ""

runAction :: Members '[Final IO, Embed IO, Resource, Logger, Output CompletedStrategy] r => Path Abs Dir -> (Action -> Sem r ()) -> Action -> Sem r ()
runAction basedir enqueue = \case
  ADiscover Discover{..} -> do
    let prettyName = fill 20 (annotate (colorDull Cyan) (pretty discoverName <> " "))

    result <- discoverFunc basedir
      & readFSToIO
      & readFSErrToCLIErr
      & execToIO
      & execErrToCLIErr
      & errorToIOFinal @CLIErr
      & fromExceptionSem @SomeException
      & errorToIOFinal @SomeException
      & runOutputSem @ConfiguredStrategy (enqueue . AStrategy)

    case result of
      Left someException -> do
        logWarn $ prettyName <> annotate (color Red) "Discovery failed with uncaught SomeException"
        logWarn $ pretty (show someException) <> line
      Right (Left err) -> do
        logWarn $ prettyName <> annotate (color Red) "Discovery failed"
        logDebug $ pretty (show err) <> line
      Right (Right ()) -> logDebug $ prettyName <> annotate (color Green) "Finished discovery"

  AStrategy (ConfiguredStrategy Strategy{..} opts) -> do
    let prettyName = annotate (color Cyan) (pretty strategyName)
        prettyPath = pretty (toFilePath (strategyModule opts))

    result <- strategyAnalyze opts
      & readFSToIO
      & readFSErrToCLIErr
      & execToIO
      & execErrToCLIErr
      & errorToIOFinal @CLIErr
      & fromExceptionSem @SomeException
      & errorToIOFinal @SomeException

    case result of
      Left someException -> do
        logWarn $ prettyPath <> " " <> prettyName <> " " <> annotate (color Yellow) "Analysis failed with uncaught SomeException"
        logDebug $ pretty (show someException) <> line
      Right (Left err) -> do
        logWarn $ prettyPath <> " " <> prettyName <> " " <> annotate (color Yellow) "Analysis failed"
        logDebug $ pretty (show err) <> line
      Right (Right graph) -> do
        logInfo $ prettyPath <> " " <> prettyName <> " " <> annotate (color Green) "Analyzed"
        logDebug (pretty (show graph))
        output (CompletedStrategy strategyName (strategyModule opts) graph strategyOptimal strategyComplete)

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
