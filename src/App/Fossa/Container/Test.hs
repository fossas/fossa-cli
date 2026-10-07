{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Container.Test (
  test,
  reportIssues,
) where

import App.Fossa.API.BuildWait (
  waitForBuild,
  waitForIssues,
 )
import App.Fossa.Config.Container (
  ContainerTestConfig (ContainerTestConfig, timeoutDuration),
  TestOutputFormat (TestOutputJson, TestOutputPretty),
 )
import App.Fossa.Config.Container qualified as Config
import App.Fossa.Container.Scan (scanImageNoAnalysis)
import App.Fossa.PreflightChecks (PreflightCommandChecks (TestChecks), guardWithPreflightChecks)
import App.Types (LocatorType (..), OverrideProject (OverrideProject, overrideBranch, overrideName, overrideRevision), ProjectRevision (..))
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.StickyLogger (logSticky, runStickyLogger)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.Lift (Has, Lift)
import Control.Monad (void)
import Control.Timeout (timeout')
import Data.Aeson qualified as Aeson
import Data.Maybe (fromMaybe)
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text)
import Data.Text.Extra (showT)
import Effect.Exec (Exec)
import Effect.Logger (
  Logger,
  Pretty (pretty),
  Severity (SevInfo),
  logError,
  logInfo,
  logStdout,
 )
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (Issues (..))

extractRevision :: OverrideProject -> Text -> Text -> ProjectRevision
extractRevision OverrideProject{..} imageTag imageDigest =
  ProjectRevision
    (fromMaybe imageTag overrideName)
    (fromMaybe imageDigest overrideRevision)
    overrideBranch

test ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  ContainerTestConfig ->
  m ()
test ContainerTestConfig{..} = do
  void $ guardWithPreflightChecks apiOpts TestChecks

  runStickyLogger SevInfo
    . ignoreDebug -- Ignore the debug effect because we don't generate a bundle.
    . runFossaApiClient apiOpts
    . timeout' timeoutDuration
    $ \cancelToken -> do
      (imageTag, imageDigest) <- scanImageNoAnalysis testImageLocator testDockerHost testArch
      let revision = extractRevision testRevisionOverride imageTag imageDigest

      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using project revision: `" <> pretty (projectRevision revision) <> "`")

      logSticky "[ Waiting for build completion ]"
      waitForBuild revision LocatorTypeCustom cancelToken

      logSticky "[ Waiting for issue scan completion ]"
      issues <- waitForIssues revision Nothing LocatorTypeCustom cancelToken
      logSticky ""

      reportIssues outputFormat issues

-- | Report the issues found for the scanned image, failing the command when
-- there are any.
--
-- The failure goes through 'Diagnostics' (as @fossa test@ does) rather than
-- 'System.Exit.exitFailure': the subcommand runner catches every synchronous
-- exception thrown inside its effect stack, @ExitCode@ included, and reports it
-- as a diagnostic, so exiting from here surfaced a spurious
-- @An exception occurred:ExitFailure 1@ instead of the failure message.
reportIssues ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  TestOutputFormat ->
  Issues ->
  m ()
reportIssues outputFormat issues =
  case issuesCount issues of
    0 -> logInfo "Test passed! 0 issues found"
    n -> do
      logError $ "Test failed. Number of issues found: " <> pretty n
      if null (issuesIssues issues)
        then logError "Check webapp for more details, or use a full-access API key (currently using a push-only API key)"
        else case outputFormat of
          TestOutputPretty -> logError $ pretty issues
          TestOutputJson -> logStdout . decodeUtf8 . Aeson.encode $ issues
      fatalText $ "The scan has revealed issues. Number of issues found: " <> showT n
