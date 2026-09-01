module App.Fossa.Ficus.Workflow (
  analyzeWithWorkflow,
  -- Exported for testing: everything except which binary runs the workflow.
  runWorkflowWith,
) where

import App.Fossa.EmbeddedBinary (toPath, withFicusBinary)
import App.Fossa.Ficus.Analyze (execFicusStreaming)
import App.Fossa.Ficus.Types (
  FicusMessage (FicusMessageFinding),
  WorkflowEvent (..),
  WorkflowRunArtifact (..),
  findingToWorkflowEvent,
  toWorkflowExecutable,
  workflowResultJson,
 )
import Control.Effect.Debug (Debug, debugMetadata)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.Lift (Has, Lift)
import Control.Effect.Path (withSystemTempDir)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Map qualified as Map
import Data.String.Conversion (decodeUtf8, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Effect.Exec (AllowErr (Never), Command (..), ExitCode (ExitSuccess))
import Effect.Logger (Logger, logDebug, logError, logInfo, pretty)
import Path (Abs, Dir, File, Path, toFilePath)

-- | Run the workflow analyzer at the given path over @target@ through
-- @ficus x-workflow@, streaming its observations. Nothing is uploaded: the
-- result lands in the debug bundle and nowhere else.
analyzeWithWorkflow ::
  ( Has Debug sig m
  , Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  Path Abs Dir ->
  Path Abs File ->
  Maybe FilePath ->
  m ()
analyzeWithWorkflow target analyzer maybeDebugDir =
  withFicusBinary $ \bin ->
    runWorkflowWith (workflowCommand . toText $ toPath bin) target analyzer maybeDebugDir

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

runWorkflowWith ::
  ( Has Debug sig m
  , Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  Command ->
  Path Abs Dir ->
  Path Abs File ->
  Maybe FilePath ->
  m ()
runWorkflowWith cmd target analyzer maybeDebugDir =
  -- The child writes a step cache and a per-run temp directory relative to its
  -- working directory, so it must not be the repository under analysis.
  withSystemTempDir "fossa-workflow" $ \scratch -> do
    let artifact = WorkflowRunArtifact (toWorkflowExecutable analyzer) target scratch
        artifactBytes = BL.toStrict $ Aeson.encode artifact
    logDebug $ "Workflow run artifact: " <> pretty (decodeUtf8 artifactBytes :: Text)

    (events, exitCode, stdErrLines) <-
      execFicusStreaming scratch cmd (Just artifactBytes) maybeDebugDir "fossa.ficus-workflow" collectWorkflowEvent []

    traverse_ reportEvent events
    case (exitCode, [value | WorkflowResult value <- events]) of
      (ExitSuccess, result : _) -> do
        debugMetadata workflowResultJson result
        logDebug $ "Workflow result: " <> pretty (decodeUtf8 (Aeson.encode result) :: Text)
        logInfo "Workflow analysis complete"
      _ -> failWorkflow analyzer exitCode events stdErrLines

collectWorkflowEvent :: [WorkflowEvent] -> FicusMessage -> IO [WorkflowEvent]
collectWorkflowEvent acc (FicusMessageFinding finding) =
  pure $ maybe acc ((acc <>) . pure) (findingToWorkflowEvent finding)
collectWorkflowEvent acc _ = pure acc

reportEvent :: (Has Logger sig m) => WorkflowEvent -> m ()
reportEvent = \case
  WorkflowStarted resolvedProgram analyzerVersion ->
    logInfo $ "Running workflow analyzer " <> pretty analyzerVersion <> " (" <> pretty resolvedProgram <> ")"
  WorkflowStepCompleted step -> logInfo $ "  " <> pretty step <> " completed"
  WorkflowResult _ -> pure ()
  WorkflowFailed reason _ -> logError $ "Workflow analyzer failed: " <> pretty reason

-- | Fatal on a non-zero exit or a missing result. ficus emits one terminal
-- observation per run, so an observation usually supplies the reason, but the
-- exit code is the signal a truncated stream cannot lose and is what decides.
-- A clean exit with no result is a bug, not an empty answer: the user asked for
-- this run by naming a path, so it must not pass silently.
failWorkflow ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  Path Abs File ->
  ExitCode ->
  [WorkflowEvent] ->
  [Text] ->
  m ()
failWorkflow analyzer exitCode events stdErrLines = do
  logError . pretty $ Text.unlines (summary : stderrTail)
  fatalText summary
  where
    summary :: Text
    summary =
      "The workflow analyzer at "
        <> toText (toFilePath analyzer)
        <> " did not produce a result ("
        <> reason
        <> ")."

    reason :: Text
    reason = case ([r | WorkflowFailed r _ <- events], exitCode) of
      (failure : _, _) -> failure
      ([], ExitSuccess) -> "ficus exited successfully but reported no result"
      ([], code) -> "ficus exited with " <> toText (show code)

    stderrTail :: [Text]
    stderrTail
      | null stdErrLines = []
      | otherwise = "==== BEGIN ficus stderr ====" : stdErrLines <> ["==== END ficus stderr ===="]
