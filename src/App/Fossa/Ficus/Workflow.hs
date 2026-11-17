{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Ficus.Workflow (
  analyzeWithWorkflow,
  findingsFileName,
  writeFindingsDoc,
  -- Also exported for testing.
  runWorkflowWith,
) where

import App.Fossa.EmbeddedBinary (toPath, withFicusBinary)
import App.Fossa.Ficus.Analyze (execFicusStreaming)
import App.Fossa.Ficus.Types (
  FicusMessage (FicusMessageFinding),
  WorkflowEvent (..),
  WorkflowRunArtifact (..),
  downloadedWorkflowExecutable,
  findingToWorkflowEvent,
  workflowResultJson,
 )
import Control.Carrier.Diagnostics qualified as Diag
import Control.Effect.Debug (Debug, debugMetadata)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Effect.Path (withSystemTempDir)
import Control.Monad (unless)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Map qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.String.Conversion (decodeUtf8, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Diag.Result (resultToMaybe)
import Effect.Exec (AllowErr (Never), Command (..), ExitCode (ExitSuccess))
import Effect.Logger (Logger, Severity (SevDebug, SevWarn), logDebug, logError, logInfo, logWarn, pretty)
import Path (Abs, Dir, File, Path, Rel, mkRelFile, toFilePath, (</>))

-- | Download the workflow analyzer and run it over @target@ through
-- @ficus x-workflow@, streaming its observations. The result is recorded in
-- the debug bundle and returned for upload once the server has issued the
-- revision locator; nothing is sent from here. The rendered findings document,
-- when present, is written into the run's scratch dir for the child to read.
analyzeWithWorkflow ::
  ( Has Debug sig m
  , Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  Path Abs Dir ->
  Maybe FilePath ->
  Maybe Aeson.Value ->
  m Aeson.Value
analyzeWithWorkflow target maybeDebugDir findings =
  withFicusBinary $ \bin ->
    runWorkflowWith (workflowCommand . toText $ toPath bin) target maybeDebugDir findings

-- | @--config -@ puts the run artifact on stdin, so it never lands on the
-- process table.
workflowCommand :: Text -> Command
workflowCommand ficus =
  Command
    { cmdName = ficus
    , cmdArgs = ["x-workflow", "--config", "-"]
    , cmdAllowErr = Never
    , cmdEnvVars = Map.empty
    }

-- | The child runs with CWD set to the run artifact's @workingDirectory@
-- (the scratch dir), so a relative file name there is enough for it to find
-- the findings document.
findingsFileName :: Path Rel File
findingsFileName = $(mkRelFile "project-findings.json")

-- | Findings are adjunct data: a failed write degrades to "the file is absent"
-- (the caller warns about it) and the run continues.
writeFindingsDoc :: (Has (Lift IO) sig m) => Path Abs Dir -> Maybe Aeson.Value -> m ()
writeFindingsDoc scratch = traverse_ (sendIO . BL.writeFile (toFilePath (scratch </> findingsFileName)) . Aeson.encode)

runWorkflowWith ::
  ( Has Debug sig m
  , Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  Command ->
  Path Abs Dir ->
  Maybe FilePath ->
  Maybe Aeson.Value ->
  m Aeson.Value
runWorkflowWith cmd target maybeDebugDir findings =
  -- The child writes a step cache and a per-run temp directory relative to its
  -- working directory, so it must not be the repository under analysis.
  withSystemTempDir "fossa-workflow" $ \scratch -> do
    let artifact = WorkflowRunArtifact downloadedWorkflowExecutable target scratch
        artifactBytes = BL.toStrict $ Aeson.encode artifact
    logDebug $ "Workflow run artifact: " <> pretty (decodeUtf8 artifactBytes :: Text)
    writeOutcome <- Diag.errorBoundaryIO $ writeFindingsDoc scratch findings
    Diag.flushLogs SevWarn SevDebug writeOutcome
    unless (isJust (resultToMaybe writeOutcome)) $
      logWarn "Failed to write the findings document into the workflow scratch dir; the workflow will not see it"

    (collected, exitCode, stdErrLines) <-
      execFicusStreaming scratch cmd (Just artifactBytes) maybeDebugDir "fossa.ficus-workflow" collectWorkflowEvent []

    let (undecodable, events) = partitionEithers collected
    traverse_ reportUndecodable undecodable
    traverse_ reportEvent events
    case (exitCode, mapMaybe workflowResult events) of
      (ExitSuccess, result : _) -> do
        debugMetadata workflowResultJson result
        logDebug $ "Workflow result: " <> pretty (decodeUtf8 (Aeson.encode result) :: Text)
        logInfo "Workflow analysis complete"
        pure result
      _ -> failWorkflow exitCode events stdErrLines

workflowResult :: WorkflowEvent -> Maybe Aeson.Value
workflowResult (WorkflowResult value) = Just value
workflowResult _ = Nothing

failureReason :: WorkflowEvent -> Maybe Text
failureReason (WorkflowFailed reason _) = Just reason
failureReason _ = Nothing

collectWorkflowEvent :: [Either Text WorkflowEvent] -> FicusMessage -> IO [Either Text WorkflowEvent]
collectWorkflowEvent acc (FicusMessageFinding finding) =
  pure $ maybe acc ((acc <>) . pure) (findingToWorkflowEvent finding)
collectWorkflowEvent acc _ = pure acc

-- | Debug level: a payload this version cannot read is version skew, and the
-- exit code still decides the run.
reportUndecodable :: (Has Logger sig m) => Text -> m ()
reportUndecodable payload = logDebug $ "Undecodable workflow observation: " <> pretty payload

reportEvent :: (Has Logger sig m) => WorkflowEvent -> m ()
reportEvent = \case
  WorkflowStarted resolvedProgram analyzerVersion ->
    logInfo $ "Running workflow analyzer " <> pretty analyzerVersion <> " (" <> pretty resolvedProgram <> ")"
  WorkflowStepCompleted step -> logInfo $ "  " <> pretty step <> " completed"
  WorkflowResult _ -> pure ()
  -- 'failWorkflow' renders the reason and fails on it; reporting it here as
  -- well says the same thing twice before the error itself.
  WorkflowFailed{} -> pure ()

-- | Fatal on a non-zero exit or a missing result. ficus emits one terminal
-- observation per run, so an observation usually supplies the reason, but the
-- exit code is the signal a truncated stream cannot lose and is what decides.
-- A clean exit with no result is a bug, not an empty answer: the user asked for
-- this run with @--x-workflow@, so it must not pass silently.
failWorkflow ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  ExitCode ->
  [WorkflowEvent] ->
  [Text] ->
  m a
failWorkflow exitCode events stdErrLines = do
  logError . pretty $ Text.unlines (summary : stderrTail)
  fatalText summary
  where
    summary :: Text
    summary =
      "The workflow analyzer did not produce a result ("
        <> reason
        <> ")."

    reason :: Text
    reason = case (mapMaybe failureReason events, exitCode) of
      (failure : _, _) -> failure
      ([], ExitSuccess) -> "ficus exited successfully but reported no result"
      ([], code) -> "ficus exited with " <> toText (show code)

    stderrTail :: [Text]
    stderrTail
      | null stdErrLines = []
      | otherwise = "==== BEGIN ficus stderr ====" : stdErrLines <> ["==== END ficus stderr ===="]
