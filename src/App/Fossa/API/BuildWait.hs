module App.Fossa.API.BuildWait (
  waitForBuild,
  waitForMonorepoScan,
  waitForIssues,
  waitForSherlockScan,
  waitForScanCompletion,
  timeout,
) where

import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.VPS.Scan.Core qualified as VPSCore
import App.Fossa.VPS.Scan.ScotlandYard qualified as ScotlandYard
import App.Types (ProjectRevision (projectName, projectRevision))
import Control.Algebra (Has)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Effect.Diagnostics (
  Diagnostics,
  ToDiagnostic (..),
  fatal,
  fatalText,
  recover,
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.StickyLogger (StickyLogger, logSticky')
import Data.Functor (($>))
import Data.Text (Text)
import Effect.Logger (Logger, pretty, viaShow)
import Fossa.API.Types (ApiOpts, Issues (..))

pollDelaySeconds :: Int
pollDelaySeconds = 8

data WaitError
  = -- | we encountered the FAILED status on a build
    BuildFailed
  deriving (Eq, Ord, Show)

instance ToDiagnostic WaitError where
  renderDiagnostic BuildFailed = "The build failed. Check the FOSSA webapp for more details."

-- | Wait for either a normal build completion or a monorepo scan completion.
-- Try to detect the correct method, use provided fallback
waitForScanCompletion ::
  (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m, Has StickyLogger sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m ()
waitForScanCompletion apiopts revision = do
  -- Route is new, this may fail on on-prem if they haven't updated
  project <- recover $ Fossa.getProject apiopts revision

  -- Try inferring, fallback to standard.
  let runAsMonorepo = maybe False Fossa.projectIsMonorepo project

  if runAsMonorepo
    then waitForMonorepoScan apiopts revision
    else waitForBuild apiopts revision

-- | Wait for a "normal" (non-VPS) build completion
waitForBuild ::
  (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m, Has StickyLogger sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m ()
waitForBuild apiOpts revision = do
  build <- Fossa.getLatestBuild apiOpts revision

  case Fossa.buildTaskStatus (Fossa.buildTask build) of
    Fossa.StatusSucceeded -> pure ()
    Fossa.StatusFailed -> fatal BuildFailed
    otherStatus -> do
      logSticky' $ "[ Waiting for build completion... last status: " <> viaShow otherStatus <> " ]"
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForBuild apiOpts revision

-- | Wait for monorepo scan completion
waitForMonorepoScan ::
  (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m, Has StickyLogger sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m ()
waitForMonorepoScan apiOpts revision = do
  Fossa.Organization orgId _ <- Fossa.getOrganization apiOpts
  let locator = VPSCore.createLocator (projectName revision) orgId

  logSticky' "[ Getting latest scan ID ]"
  scan <- ScotlandYard.getLatestScan apiOpts locator (projectRevision revision)

  logSticky' "[ Waiting for monorepo scan... ]"
  waitForSherlockScan apiOpts locator (ScotlandYard.responseScanId scan)
  pure ()

waitForIssues ::
  (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Issues
waitForIssues apiOpts revision = do
  issues <- Fossa.getIssues apiOpts revision
  case issuesStatus issues of
    "WAITING" -> do
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForIssues apiOpts revision
    _ -> pure issues

-- | Wait for sherlock scan completion (VPS)
waitForSherlockScan ::
  (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m, Has StickyLogger sig m) =>
  ApiOpts ->
  VPSCore.Locator ->
  -- | scan ID
  Text ->
  m ()
waitForSherlockScan apiOpts locator scanId = do
  scan <- ScotlandYard.getScan apiOpts locator scanId
  case ScotlandYard.responseScanStatus scan of
    Just "AVAILABLE" -> pure ()
    Just "ERROR" -> fatalText "The component scan failed. Check the FOSSA webapp for more details."
    Just otherStatus -> do
      logSticky' $ "[ Waiting for component scan... last status: " <> pretty otherStatus <> " ]"
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForSherlockScan apiOpts locator scanId
    Nothing -> do
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForSherlockScan apiOpts locator scanId

timeout ::
  -- | number of seconds before timeout
  Int ->
  IO a ->
  IO (Maybe a)
timeout seconds act = either id id <$> Async.race (Just <$> act) (threadDelay (seconds * 1_000_000) $> Nothing)
