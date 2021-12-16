module App.Fossa.API.BuildWait (
  waitForBuild,
  waitForMonorepoScan,
  waitForIssues,
  waitForSherlockScan,
  waitForScanCompletion,
  timeout,
  timeout',
  waitForScanCompletion',
  waitForIssues',
  waitForSherlockScan',
  shouldCancel,
  Cancel,
) where

import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.VPS.Scan.Core qualified as VPSCore
import App.Fossa.VPS.Scan.ScotlandYard qualified as ScotlandYard
import App.Types (ProjectRevision (projectName, projectRevision))
import Control.Algebra (Has)
import Control.Carrier.Threaded (fork, kill)
import Control.Concurrent (MVar, newEmptyMVar, putMVar, threadDelay, tryTakeMVar)
import Control.Concurrent.Async qualified as Async
import Control.Effect.Diagnostics (
  Diagnostics,
  ToDiagnostic (..),
  fatal,
  fatalText,
  recover,
 )
import Control.Effect.Exception (Lift, finally)
import Control.Effect.Lift (sendIO)
import Control.Effect.StickyLogger (StickyLogger, logSticky')
import Control.Monad (when)
import Data.Functor (($>))
import Data.Maybe (isJust)
import Data.Text (Text)
import Effect.Logger (Logger, pretty, viaShow)
import Fossa.API.Types (ApiOpts, Issues (..))

pollDelaySeconds :: Int
pollDelaySeconds = 8

data WaitError
  = -- | We encountered the FAILED status on a build
    BuildFailed
  | -- | We ran out of time locally, and aborted
    LocalTimeout
  deriving (Eq, Ord, Show)

-- Opaque wrapper
newtype Cancel = Cancel (MVar ()) deriving (Eq)

shouldCancel :: Cancel -> IO Bool
shouldCancel (Cancel mvar) = isJust <$> tryTakeMVar mvar

instance ToDiagnostic WaitError where
  renderDiagnostic BuildFailed = "The build failed. Check the FOSSA webapp for more details."
  renderDiagnostic LocalTimeout = "Build/Issue scan was not completed, and the CLI has locally timed out."

-- | Wait for either a normal build completion or a monorepo scan completion.
-- Try to detect the correct method, use provided fallback
-- TODO: Delete me!  I'm an infinite loop!
waitForScanCompletion ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  ) =>
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
-- TODO: Delete me!  I'm an infinite loop!
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
-- TODO: Delete me!  I'm an infinite loop!
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

-- TODO: Delete me!  I'm an infinite loop!
waitForIssues ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
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
-- TODO: Delete me!  I'm an infinite loop!
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

-- | Wait for either a normal build completion or a monorepo scan completion.
-- Try to detect the correct method, use provided fallback
waitForScanCompletion' ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  ) =>
  ApiOpts ->
  ProjectRevision ->
  Cancel ->
  m ()
waitForScanCompletion' apiopts revision cancelFlag = do
  -- Route is new, this may fail on on-prem if they haven't updated
  project <- recover $ Fossa.getProject apiopts revision

  -- Try inferring, fallback to standard.
  let runAsMonorepo = maybe False Fossa.projectIsMonorepo project

  if runAsMonorepo
    then waitForMonorepoScan' apiopts revision cancelFlag
    else waitForBuild' apiopts revision cancelFlag

-- | Wait for a "normal" (non-VPS) build completion
waitForBuild' ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  ) =>
  ApiOpts ->
  ProjectRevision ->
  Cancel ->
  m ()
waitForBuild' apiOpts revision cancelFlag = do
  build <- Fossa.getLatestBuild apiOpts revision

  case Fossa.buildTaskStatus (Fossa.buildTask build) of
    Fossa.StatusSucceeded -> pure ()
    Fossa.StatusFailed -> fatal BuildFailed
    otherStatus -> do
      logSticky' $ "[ Waiting for build completion... last status: " <> viaShow otherStatus <> " ]"
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForBuild' apiOpts revision cancelFlag

-- | Wait for monorepo scan completion
waitForMonorepoScan' ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  ) =>
  ApiOpts ->
  ProjectRevision ->
  Cancel ->
  m ()
waitForMonorepoScan' apiOpts revision cancelFlag = do
  checkForCancel cancelFlag
  Fossa.Organization orgId _ <- Fossa.getOrganization apiOpts
  let locator = VPSCore.createLocator (projectName revision) orgId

  logSticky' "[ Getting latest scan ID ]"
  scan <- ScotlandYard.getLatestScan apiOpts locator (projectRevision revision)

  logSticky' "[ Waiting for monorepo scan... ]"
  waitForSherlockScan apiOpts locator (ScotlandYard.responseScanId scan)

waitForIssues' ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  ApiOpts ->
  ProjectRevision ->
  Cancel ->
  m Issues
waitForIssues' apiOpts revision cancelFlag = do
  checkForCancel cancelFlag
  issues <- Fossa.getIssues apiOpts revision
  case issuesStatus issues of
    "WAITING" -> do
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForIssues apiOpts revision
    _ -> pure issues

-- | Wait for sherlock scan completion (VPS)
waitForSherlockScan' ::
  (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m, Has StickyLogger sig m) =>
  ApiOpts ->
  VPSCore.Locator ->
  Cancel ->
  -- | scan ID
  Text ->
  m ()
waitForSherlockScan' apiOpts locator cancelFlag scanId = do
  checkForCancel cancelFlag
  scan <- ScotlandYard.getScan apiOpts locator scanId
  case ScotlandYard.responseScanStatus scan of
    Just "AVAILABLE" -> pure ()
    Just "ERROR" -> fatalText "The component scan failed. Check the FOSSA webapp for more details."
    Just otherStatus -> do
      logSticky' $ "[ Waiting for component scan... last status: " <> pretty otherStatus <> " ]"
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForSherlockScan' apiOpts locator cancelFlag scanId
    Nothing -> do
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForSherlockScan' apiOpts locator cancelFlag scanId

checkForCancel ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  Cancel ->
  m ()
checkForCancel cancel = do
  should <- sendIO $ shouldCancel cancel
  when should $ fatal LocalTimeout

timeout' :: (Has (Lift IO) sig m) => Int -> (Cancel -> m a) -> m a
timeout' seconds act = do
  mvar <- sendIO newEmptyMVar
  handle <- sendIO $
    fork $ do
      threadDelay $ seconds * 1_000_000
      putMVar mvar ()
  -- We need 'finally' here, because `act` can short-circuit.
  -- If we don't use it, we might join the thread, which
  -- requires the timeout to fully expire.
  act (Cancel mvar) `finally` kill handle
