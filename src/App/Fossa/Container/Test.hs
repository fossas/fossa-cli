{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Container.Test (
  test,
) where

import App.Fossa.API.BuildWait (
  timeout',
  waitForBuild',
  waitForIssues',
 )
import App.Fossa.Container.Scan (
  extractRevision,
  runSyft,
  toContainerScan,
 )
import App.NewFossa.Config.Container (
  ContainerTestConfig (ContainerTestConfig, timeoutDuration),
  OutputFormat (TestOutputJson, TestOutputPretty),
 )
import App.NewFossa.Config.Container qualified as Config
import App.Types (ProjectRevision (..))
import Control.Carrier.StickyLogger (logSticky, runStickyLogger)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift, sendIO)
import Data.Aeson qualified as Aeson
import Data.String.Conversion (decodeUtf8)
import Effect.Logger (
  Logger,
  Pretty (pretty),
  Severity (SevInfo),
  logError,
  logInfo,
  logStdout,
 )
import Fossa.API.Types (Issues (..))
import System.Exit (exitFailure)

test ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  ContainerTestConfig ->
  m ()
test ContainerTestConfig{..} = runStickyLogger SevInfo $
  timeout' timeoutDuration $ \cancelToken -> do
    logSticky "Running embedded syft binary"

    containerScan <- runSyft testImageLocator >>= toContainerScan
    let revision = extractRevision testRevisionOverride containerScan

    logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
    logInfo ("Using project revision: `" <> pretty (projectRevision revision) <> "`")

    logSticky "[ Waiting for build completion ]"
    waitForBuild' apiOpts revision cancelToken

    logSticky "[ Waiting for issue scan completion ]"
    issues <- waitForIssues' apiOpts revision cancelToken
    logSticky ""

    case issuesCount issues of
      0 -> logInfo "Test passed! 0 issues found"
      n -> do
        logError $ "Test failed. Number of issues found: " <> pretty n
        if null (issuesIssues issues)
          then logError "Check webapp for more details, or use a full-access API key (currently using a push-only API key)"
          else do
            case outputFormat of
              TestOutputPretty -> logError $ pretty issues
              TestOutputJson -> logStdout . decodeUtf8 . Aeson.encode $ issues
            sendIO exitFailure
