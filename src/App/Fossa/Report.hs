{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Report (
  report,
  fetchReport,
  reportSubCommand,
) where

import App.Fossa.API.BuildWait (
  waitForReportReadiness,
  waitForScanCompletion,
 )
import App.Fossa.Config.Report (ReportCliOptions, ReportConfig (..), ReportOutputFormat (ReportJson), mkSubCommand)
import App.Fossa.PreflightChecks (PreflightCommandChecks (ReportChecks), guardWithPreflightChecks)
import App.Fossa.Subcommand (SubCommand)
import App.Types (ProjectRevision (..))
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.StickyLogger (StickyLogger, logSticky, runStickyLogger)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (FossaApiClient, getAttribution)
import Control.Effect.Lift (Has, Lift)
import Control.Monad (void, when)
import Control.Timeout (timeout')
import Data.String.Conversion (toText)
import Data.Text.Extra (showT)
import Effect.Logger (
  Logger,
  Pretty (pretty),
  Severity (SevInfo),
  logInfo,
  logStdout,
  logWarn,
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

  void $ guardWithPreflightChecks apiOpts ReportChecks

  runStickyLogger SevInfo
    . ignoreDebug
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
      when (outputFormat /= ReportJson) $
        logWarn (pretty $ "\"" <> toText outputFormat <> "\" format may change independent of CLI version: it is sourced from the FOSSA service.")

      logSticky "[ Waiting for build completion... ]"

      waitForScanCompletion revision cancelToken
      logSticky "[ Waiting for scan completion... ]"

      waitForReportReadiness revision cancelToken
      logSticky $ "[ Fetching " <> showT reportType <> " report... ]"

      renderedReport <- getAttribution revision outputFormat
      logStdout renderedReport
