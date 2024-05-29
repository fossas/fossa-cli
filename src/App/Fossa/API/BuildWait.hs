{-# LANGUAGE OverloadedRecordDot #-}

module App.Fossa.API.BuildWait (
  waitForScanCompletion,
  waitForIssues,
  waitForBuild,
  waitForReportReadiness,
) where

import App.Fossa.Config.Test (DiffRevision)
import App.Types (IssueLocatorType (..), ProjectRevision, projectRevision)
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
import Control.Effect.StickyLogger (StickyLogger, logSticky, logSticky')
import Control.Monad (void, when)
import Control.Timeout (Cancel, checkForCancel, delay)
import Data.Error (SourceLocation, createEmptyBlock, getSourceLocation)
import Data.String.Conversion (showText)
import Effect.Logger (Logger, viaShow)
import Errata (errataSimple)
import Errata.Types (Errata)
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
    BuildFailed SourceLocation
  | -- | We ran out of time locally, and aborted
    LocalTimeout SourceLocation
  deriving (Eq, Ord, Show)

instance ToDiagnostic WaitError where
  renderDiagnostic :: WaitError -> Errata
  renderDiagnostic (BuildFailed srcLoc) =
    errataSimple (Just "The build failed. Check the FOSSA webapp for more details") (createEmptyBlock srcLoc) Nothing
  renderDiagnostic (LocalTimeout srcLoc) =
    errataSimple (Just "Build/Issue scan was not completed on the FOSSA server, and the --timeout duration has expired") (createEmptyBlock srcLoc) Nothing

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
  IssueLocatorType ->
  Cancel ->
  m ()
waitForScanCompletion revision locatorType cancelFlag = do
  -- Route is new, this may fail on on-prem if they haven't updated
  project <- recover $ getProject revision locatorType
  if maybe False projectIsMonorepo project
    then fatalText "The project you are attempting to test is a monorepo project. Monorepo projects are no longer supported by FOSSA."
    else waitForBuild revision locatorType cancelFlag

waitForIssues ::
  ( Has Diagnostics sig m
  , Has FossaApiClient sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  ) =>
  ProjectRevision ->
  Maybe DiffRevision ->
  IssueLocatorType ->
  Cancel ->
  m Issues
waitForIssues revision diffRevision locatorType cancelFlag = do
  checkForTimeout cancelFlag
  issues <- getIssues revision diffRevision locatorType
  case issuesStatus issues of
    "WAITING" -> do
      pauseForRetry
      waitForIssues revision diffRevision locatorType cancelFlag
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
  IssueLocatorType ->
  Cancel ->
  m ()
waitForBuild revision locatorType cancelFlag = do
  checkForTimeout cancelFlag
  build <- getLatestBuild revision locatorType

  case buildTaskStatus (buildTask build) of
    StatusSucceeded -> pure ()
    StatusFailed -> fatal $ BuildFailed getSourceLocation
    otherStatus -> do
      logSticky $ "[ Waiting for build completion (revision " <> revision.projectRevision <> ")... last status: " <> showText otherStatus <> " ]"
      pauseForRetry
      waitForBuild revision locatorType cancelFlag

-- | Specialized version of 'checkForCancel' which represents
-- a backend build/issue scan timeout.
checkForTimeout ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  Cancel ->
  m ()
checkForTimeout = checkForCancel $ LocalTimeout getSourceLocation

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
  void $ waitForIssues revision Nothing IssueLocatorCustom cancelFlag

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
