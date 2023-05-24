module App.Fossa.API.BuildWait (
  waitForScanCompletion,
  waitForIssues,
  waitForBuild,
  waitForReportReadiness,
) where

import App.Fossa.Config.Test (DiffRevision)
import App.Types (ProjectRevision)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  ToDiagnostic (..),
  fatal,
  fatalText,
  recover,
 )
import Control.Effect.Exception (Lift)
import Control.Effect.FossaApiClient (
  FossaApiClient,
  getApiOpts,
  getIssues,
  getLatestBuild,
  getOrganization,
  getProject,
  getRevisionDependencyCacheStatus,
 )
import Control.Effect.StickyLogger (StickyLogger, logSticky')
import Control.Monad (void, when)
import Control.Timeout (Cancel, checkForCancel, delay)
import Effect.Logger (Logger, viaShow)
import Fossa.API.Types (
  ApiOpts (apiOptsPollDelay),
  Build (buildTask),
  BuildStatus (StatusFailed, StatusSucceeded),
  BuildTask (buildTaskStatus),
  Issues (issuesStatus),
  Organization (orgSupportsDependenciesCachePolling),
  Project (projectIsMonorepo),
  RevisionDependencyCache (status),
  RevisionDependencyCacheStatus (Ready, UnknownDependencyCacheStatus, Waiting),
 )

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
  , Has Logger sig m
  , Has StickyLogger sig m
  , Has FossaApiClient sig m
  , Has (Lift IO) sig m
  ) =>
  ProjectRevision ->
  Cancel ->
  m ()
waitForScanCompletion revision cancelFlag = do
  -- Route is new, this may fail on on-prem if they haven't updated
  project <- recover $ getProject revision
  if maybe False projectIsMonorepo project
    then fatalText "The project you are attempting to test is a monorepo project. Monorepo projects are no longer supported by FOSSA."
    else waitForBuild revision cancelFlag

waitForIssues ::
  ( Has Diagnostics sig m
  , Has FossaApiClient sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  ) =>
  ProjectRevision ->
  Maybe DiffRevision ->
  Cancel ->
  m Issues
waitForIssues revision diffRevision cancelFlag = do
  checkForTimeout cancelFlag
  issues <- getIssues revision diffRevision
  case issuesStatus issues of
    "WAITING" -> do
      pauseForRetry
      waitForIssues revision diffRevision cancelFlag
    _ -> pure issues

-- | Wait for build completion
waitForBuild ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  , Has FossaApiClient sig m
  , Has (Lift IO) sig m
  ) =>
  ProjectRevision ->
  Cancel ->
  m ()
waitForBuild revision cancelFlag = do
  checkForTimeout cancelFlag
  build <- getLatestBuild revision

  case buildTaskStatus (buildTask build) of
    StatusSucceeded -> pure ()
    StatusFailed -> fatal BuildFailed
    otherStatus -> do
      logSticky' $ "[ Waiting for build completion... last status: " <> viaShow otherStatus <> " ]"
      pauseForRetry
      waitForBuild revision cancelFlag

-- | Specialized version of 'checkForCancel' which represents
-- a backend build/issue scan timeout.
checkForTimeout ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  Cancel ->
  m ()
checkForTimeout = checkForCancel LocalTimeout

pauseForRetry ::
  ( Has (Lift IO) sig m
  , Has FossaApiClient sig m
  ) =>
  m ()
pauseForRetry = do
  apiOpts <- getApiOpts
  delay $ apiOptsPollDelay apiOpts

waitForReportReadiness ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  , Has FossaApiClient sig m
  , Has (Lift IO) sig m
  ) =>
  ProjectRevision ->
  Cancel ->
  m ()
waitForReportReadiness revision cancelFlag = do
  void $ waitForIssues revision Nothing cancelFlag

  supportsDepCacheReadinessPolling <- orgSupportsDependenciesCachePolling <$> getOrganization
  when supportsDepCacheReadinessPolling $
    waitForValidDependenciesCache revision cancelFlag

waitForValidDependenciesCache ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  , Has FossaApiClient sig m
  , Has (Lift IO) sig m
  ) =>
  ProjectRevision ->
  Cancel ->
  m ()
waitForValidDependenciesCache revision cancelFlag = do
  checkForTimeout cancelFlag
  cacheStatus <- getRevisionDependencyCacheStatus revision

  case status cacheStatus of
    Ready -> pure ()
    Waiting -> do
      logSticky' $ "[ Waiting for revision's dependency cache... last status: " <> viaShow Waiting <> " ]"
      pauseForRetry
      waitForValidDependenciesCache revision cancelFlag
    UnknownDependencyCacheStatus status -> fatalText $ "unknown status of " <> status <> " received for revision's dependency cache"
