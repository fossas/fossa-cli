{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Container.Test (
  test,
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
import App.Fossa.PreflightChecks (guardWithPreflightChecks, preflightChecks)
import App.Types (OverrideProject (OverrideProject, overrideBranch, overrideName, overrideRevision), ProjectRevision (..))
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.StickyLogger (logSticky, runStickyLogger)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (void)
import Control.Timeout (timeout')
import Data.Aeson qualified as Aeson
import Data.Maybe (fromMaybe)
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text)
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
import System.Exit (exitFailure)

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
  void $ guardWithPreflightChecks apiOpts

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
      waitForBuild revision cancelToken

      logSticky "[ Waiting for issue scan completion ]"
      issues <- waitForIssues revision Nothing cancelToken
      logSticky ""

      case issuesCount issues of
        0 -> logInfo "Test passed! 0 issues found"
        n -> do
          logError $ "Test failed. Number of issues found: " <> pretty n
          if null (issuesIssues issues)
            then logError "Check webapp for more details, or use a full-access API key (currently using a push-only API key)"
            else case outputFormat of
              TestOutputPretty -> logError $ pretty issues
              TestOutputJson -> logStdout . decodeUtf8 . Aeson.encode $ issues
          sendIO exitFailure
