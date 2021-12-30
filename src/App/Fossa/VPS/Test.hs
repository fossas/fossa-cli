{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.Test (
  testMain,
) where

import App.Fossa.API.BuildWait (
  waitForIssues',
  waitForSherlockScan',
 )
import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.ProjectInference ()
import App.Fossa.VPS.Scan.Core qualified as VPSCore
import App.Fossa.VPS.Scan.ScotlandYard qualified as ScotlandYard
import App.NewFossa.Config.VPS (OutputFormat (..), TestConfig (..))
import App.Types (ProjectRevision (projectName, projectRevision))
import Control.Carrier.StickyLogger (logSticky, runStickyLogger)
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Effect.Lift (Lift, sendIO)
import Control.Timeout (timeout')
import Data.Aeson qualified as Aeson
import Data.String.Conversion (ConvertUtf8 (decodeUtf8))
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

testMain ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  TestConfig ->
  m ()
testMain TestConfig{..} = runStickyLogger SevInfo . timeout' testTimeoutDuration $ \cancelToken -> do
  logInfo ""
  logInfo ("Using project name: `" <> pretty (projectName testRevision) <> "`")
  logInfo ("Using revision: `" <> pretty (projectRevision testRevision) <> "`")

  logSticky "[ Getting latest scan ID ]"

  Fossa.Organization orgId _ <- Fossa.getOrganization testApiOpts
  let locator = VPSCore.createLocator (projectName testRevision) orgId

  scan <- ScotlandYard.getLatestScan testApiOpts locator (projectRevision testRevision)

  logSticky "[ Waiting for component scan... ]"

  waitForSherlockScan' testApiOpts locator cancelToken $ ScotlandYard.responseScanId scan

  logSticky "[ Waiting for issue scan completion... ]"
  issues <- waitForIssues' testApiOpts testRevision cancelToken
  logSticky ""

  case issuesCount issues of
    0 -> do
      logInfo "Test passed! 0 issues found"
      case testOutputFormat of
        TestOutputJson -> logStdout . decodeUtf8 . Aeson.encode $ issues
        TestOutputPretty -> pure ()
    n -> do
      logError $ "Test failed. Number of issues found: " <> pretty n
      if null (issuesIssues issues)
        then logError "Check the webapp for more details, or use a full-access API key (currently using a push-only API key)"
        else case testOutputFormat of
          TestOutputPretty -> pure ()
          TestOutputJson -> logStdout . decodeUtf8 . Aeson.encode $ issues
      sendIO exitFailure
