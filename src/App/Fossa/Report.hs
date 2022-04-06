{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Report (
  report,
  fetchReport,
  reportSubCommand,
) where

import App.Fossa.API.BuildWait (
  waitForIssues,
  waitForScanCompletion,
 )
import App.Fossa.Config.Report (ReportCliOptions, ReportConfig (..), mkSubCommand)
import App.Fossa.Subcommand (SubCommand)
import App.Types (ProjectRevision (..))
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.StickyLogger (StickyLogger, logSticky, runStickyLogger)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (FossaApiClient, getAttribution)
import Control.Effect.Lift (Has, Lift)
import Control.Timeout (timeout')
import Data.Text.Extra (showT)
import Effect.Logger (
  Logger,
  Pretty (pretty),
  Severity (SevInfo),
  logInfo,
  logStdout,
 )

reportSubCommand :: SubCommand ReportCliOptions ReportConfig
reportSubCommand = mkSubCommand report

report ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  ReportConfig ->
  m ()
report config@ReportConfig{..} = do
  -- TODO: refactor this code duplicate from `fossa test`
  {-
  Most of this module (almost everything below this line) has been copied
  from App.Fossa.Test.  I wanted to push this out sooner, and refactoring
  everything right away was not appropriate for the timing of this command.

  * Waiting for builds and issue scans (separately, but also together)
    * Above includes errors, types, and scaffolding
  -}
  runStickyLogger SevInfo
    . runFossaApiClient apiOpts
    $ fetchReport config

fetchReport ::
  ( Has Diagnostics sig m
  , Has FossaApiClient sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  , Has (Lift IO) sig m
  ) =>
  ReportConfig ->
  m ()
fetchReport ReportConfig{..} =
  timeout' timeoutDuration $
    \cancelToken -> do
      logInfo ""
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")

      logSticky "[ Waiting for build completion... ]"

      waitForScanCompletion revision cancelToken

      logSticky "[ Waiting for issue scan completion... ]"

      _ <- waitForIssues revision cancelToken

      logSticky $ "[ Fetching " <> showT reportType <> " report... ]"

      renderedReport <- getAttribution revision outputFormat

      logStdout renderedReport
