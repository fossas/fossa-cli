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
  OutputFormat (TestOutputJson, TestOutputPretty),
 )
import App.Fossa.Config.Container qualified as Config
import App.Fossa.Container.Scan (
  extractRevision,
  runSyft,
  toContainerScan,
 )
import App.Types (ProjectRevision (..))
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.StickyLogger (logSticky, runStickyLogger)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Timeout (timeout')
import Data.Aeson qualified as Aeson
import Data.String.Conversion (decodeUtf8)
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

test ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  ContainerTestConfig ->
  m ()
test ContainerTestConfig{..} = runStickyLogger SevInfo $
  runFossaApiClient apiOpts . timeout' timeoutDuration $ \cancelToken -> do
    logSticky "Running embedded syft binary"

    containerScan <- runSyft testImageLocator >>= toContainerScan
    let revision = extractRevision testRevisionOverride containerScan

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
