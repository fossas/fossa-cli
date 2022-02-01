{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.Report (
  reportMain,
  ReportType (..),
) where

import App.Fossa.API.BuildWait (
  waitForIssues,
  waitForSherlockScan,
 )
import App.Fossa.Config.VPS (ReportConfig (..), ReportType (Attribution))
import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.VPS.Scan.Core qualified as VPSCore
import App.Fossa.VPS.Scan.ScotlandYard qualified as ScotlandYard
import App.Types (ProjectRevision (projectName, projectRevision))
import Control.Carrier.StickyLogger (logSticky, runStickyLogger)
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Effect.Lift (Lift)
import Control.Timeout (timeout')
import Data.Aeson qualified as Aeson
import Data.String.Conversion (decodeUtf8)
import Data.Text.Extra (showT)
import Effect.Logger (Logger, Severity (SevInfo), logStdout)

reportMain ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  ) =>
  ReportConfig ->
  m ()
reportMain ReportConfig{..} = runStickyLogger SevInfo . timeout' reportTimeoutDuration $ \cancelToken -> do
  -- TODO: refactor this code duplicate from `fossa test`
  {-
  Most of this module (almost everything below this line) has been copied
  from App.Fossa.Test.  I wanted to push this out sooner, and refactoring
  everything right away was not appropriate for the timing of this command.

  Main points of refactor:
  * Waiting for builds and issue scans (separately, but also together)
    * Above includes errors, types, and scaffolding
  * Timeout over `IO a` (easy to move, but where do we move it?)
  * CLI command refactoring as laid out in https://github.com/fossas/issues/issues/129
  -}
  logSticky "[ Getting latest scan ID ]"

  Fossa.Organization orgId _ <- Fossa.getOrganization reportApiOpts
  let locator = VPSCore.createLocator (projectName reportRevision) orgId

  scan <- ScotlandYard.getLatestScan reportApiOpts locator (projectRevision reportRevision)

  logSticky "[ Waiting for component scan... ]"

  waitForSherlockScan reportApiOpts locator cancelToken $ ScotlandYard.responseScanId scan

  logSticky "[ Waiting for issue scan completion... ]"
  _ <- waitForIssues reportApiOpts reportRevision cancelToken

  logSticky $ "[ Fetching " <> showT reportType <> " report... ]"
  jsonValue <- case reportType of
    Attribution ->
      Fossa.getAttributionRaw reportApiOpts reportRevision

  logStdout . decodeUtf8 $ Aeson.encode jsonValue
