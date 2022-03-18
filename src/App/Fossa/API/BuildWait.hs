module App.Fossa.API.BuildWait (
  waitForScanCompletion,
  waitForIssues,
  waitForSherlockScan,
  waitForBuild,
) where

import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.VPS.Scan.Core qualified as VPSCore
import App.Fossa.VPS.Scan.ScotlandYard qualified as ScotlandYard
import App.Types (ProjectRevision (projectName, projectRevision))
import Control.Concurrent (threadDelay)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  ToDiagnostic (..),
  fatal,
  fatalText,
  recover,
 )
import Control.Effect.Exception (Lift)
import Control.Effect.Lift (sendIO)
import Control.Effect.StickyLogger (StickyLogger, logSticky')
import Control.Timeout (Cancel, checkForCancel)
import Data.Text (Text)
import Effect.Logger (Logger, pretty, viaShow)
import Fossa.API.Types (
  ApiOpts,
  Build (buildTask),
  BuildStatus (StatusFailed, StatusSucceeded),
  BuildTask (buildTaskStatus),
  Issues (issuesStatus),
  Organization (organizationId),
  Project (projectIsMonorepo),
 )

pollDelaySeconds :: Int
pollDelaySeconds = 8

data WaitError
  = -- | We encountered the FAILED status on a build
    BuildFailed
  | -- | We ran out of time locally, and aborted
    LocalTimeout
  deriving (Eq, Ord, Show)

instance ToDiagnostic WaitError where
  renderDiagnostic BuildFailed = "The build failed. Check the FOSSA webapp for more details."
  renderDiagnostic LocalTimeout = "Build/Issue scan was not completed on the FOSSA server, and the --timeout duration has expired."

-- | Wait for either a normal build completion or a monorepo scan completion.
-- Try to detect the correct method, use provided fallback
waitForScanCompletion ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  ) =>
  ApiOpts ->
  ProjectRevision ->
  Cancel ->
  m ()
waitForScanCompletion apiopts revision cancelFlag = do
  -- Route is new, this may fail on on-prem if they haven't updated
  project <- recover $ Fossa.getProject apiopts revision

  -- Try inferring, fallback to standard.
  let runAsMonorepo = maybe False projectIsMonorepo project

  if runAsMonorepo
    then waitForMonorepoScan apiopts revision cancelFlag
    else waitForBuild apiopts revision cancelFlag

-- | Wait for a "normal" (non-VPS) build completion
waitForBuild ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  ) =>
  ApiOpts ->
  ProjectRevision ->
  Cancel ->
  m ()
waitForBuild apiOpts revision cancelFlag = do
  checkForTimeout cancelFlag
  build <- Fossa.getLatestBuild apiOpts revision

  case buildTaskStatus (buildTask build) of
    StatusSucceeded -> pure ()
    StatusFailed -> fatal BuildFailed
    otherStatus -> do
      logSticky' $ "[ Waiting for build completion... last status: " <> viaShow otherStatus <> " ]"
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForBuild apiOpts revision cancelFlag

-- | Wait for monorepo scan completion
waitForMonorepoScan ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  ) =>
  ApiOpts ->
  ProjectRevision ->
  Cancel ->
  m ()
waitForMonorepoScan apiOpts revision cancelFlag = do
  checkForTimeout cancelFlag
  orgId <- organizationId <$> Fossa.getOrganization apiOpts
  let locator = VPSCore.createLocator (projectName revision) orgId

  logSticky' "[ Getting latest scan ID ]"
  scan <- ScotlandYard.getLatestScan apiOpts locator (projectRevision revision)

  logSticky' "[ Waiting for monorepo scan... ]"
  waitForSherlockScan apiOpts locator cancelFlag (ScotlandYard.responseScanId scan)

waitForIssues ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  ApiOpts ->
  ProjectRevision ->
  Cancel ->
  m Issues
waitForIssues apiOpts revision cancelFlag = do
  checkForTimeout cancelFlag
  issues <- Fossa.getIssues apiOpts revision
  case issuesStatus issues of
    "WAITING" -> do
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForIssues apiOpts revision cancelFlag
    _ -> pure issues

-- | Wait for sherlock scan completion (VPS)
waitForSherlockScan ::
  (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m, Has StickyLogger sig m) =>
  ApiOpts ->
  VPSCore.Locator ->
  Cancel ->
  -- | scan ID
  Text ->
  m ()
waitForSherlockScan apiOpts locator cancelFlag scanId = do
  checkForTimeout cancelFlag
  scan <- ScotlandYard.getScan apiOpts locator scanId
  case ScotlandYard.responseScanStatus scan of
    Just "AVAILABLE" -> pure ()
    Just "ERROR" -> fatalText "The component scan failed. Check the FOSSA webapp for more details."
    Just otherStatus -> do
      logSticky' $ "[ Waiting for component scan... last status: " <> pretty otherStatus <> " ]"
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForSherlockScan apiOpts locator cancelFlag scanId
    Nothing -> do
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForSherlockScan apiOpts locator cancelFlag scanId

-- | Specialized version of 'checkForCancel' which represents
-- a backend build/issue scan timeout.
checkForTimeout ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  Cancel ->
  m ()
checkForTimeout = checkForCancel LocalTimeout
