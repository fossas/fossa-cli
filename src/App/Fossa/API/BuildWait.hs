module App.Fossa.API.BuildWait (
  waitForScanCompletion,
  waitForIssues,
  waitForBuild,
) where

import App.Fossa.Config.Test (DiffRevision)
import App.Types (ProjectRevision (projectName))
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
  getLatestScan,
  getOrganization,
  getProject,
  getScan,
 )
import Control.Effect.StickyLogger (StickyLogger, logSticky')
import Control.Timeout (Cancel, checkForCancel, delay)
import Data.Text (Text)
import Data.Text.Extra (showT)
import Effect.Logger (Logger, pretty, viaShow)
import Fossa.API.Types (
  ApiOpts (apiOptsPollDelay),
  Build (buildTask),
  BuildStatus (StatusFailed, StatusSucceeded),
  BuildTask (buildTaskStatus),
  Issues (issuesStatus),
  OrgId,
  Organization (organizationId),
  Project (projectIsMonorepo),
  ScanId,
  ScanResponse (..),
 )
import Srclib.Types (Locator (..))

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

  -- Try inferring, fallback to standard.
  let runAsMonorepo = maybe False projectIsMonorepo project

  if runAsMonorepo
    then waitForMonorepoScan revision cancelFlag
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
waitForIssues revision maybeDiffRevision cancelFlag = do
  checkForTimeout cancelFlag
  issues <- getIssues revision maybeDiffRevision
  case issuesStatus issues of
    "WAITING" -> do
      pauseForRetry
      waitForIssues revision maybeDiffRevision cancelFlag
    _ -> pure issues

-- | Wait for a "normal" (non-VPS) build completion
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

-- | Wait for monorepo scan completion
waitForMonorepoScan ::
  ( Has Diagnostics sig m
  , Has FossaApiClient sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  , Has (Lift IO) sig m
  ) =>
  ProjectRevision ->
  Cancel ->
  m ()
waitForMonorepoScan revision cancelFlag = do
  checkForTimeout cancelFlag
  orgId <- organizationId <$> getOrganization
  let locator = createCustomLocator (projectName revision) orgId

  logSticky' "[ Getting latest scan ID ]"
  scan <- getLatestScan locator revision

  logSticky' "[ Waiting for monorepo scan... ]"
  waitForScotlandYardScan locator cancelFlag (responseScanId scan)

-- | Wait for Scotland Yard scan completion (VPS)
waitForScotlandYardScan ::
  ( Has Diagnostics sig m
  , Has FossaApiClient sig m
  , Has Logger sig m
  , Has StickyLogger sig m
  , Has (Lift IO) sig m
  ) =>
  Locator ->
  Cancel ->
  ScanId ->
  m ()
waitForScotlandYardScan locator cancelFlag scanId = do
  checkForTimeout cancelFlag
  scan <- getScan locator scanId
  case responseScanStatus scan of
    Just "AVAILABLE" -> pure ()
    Just "ERROR" -> fatalText "The component scan failed. Check the FOSSA webapp for more details."
    Just otherStatus -> do
      logSticky' $ "[ Waiting for component scan... last status: " <> pretty otherStatus <> " ]"
      pauseForRetry
      waitForScotlandYardScan locator cancelFlag scanId
    Nothing -> do
      pauseForRetry
      waitForScotlandYardScan locator cancelFlag scanId

createCustomLocator :: Text -> OrgId -> Locator
createCustomLocator projectName organizationId =
  Locator
    { locatorFetcher = "custom"
    , locatorProject = showT organizationId <> "/" <> projectName
    , locatorRevision = Nothing
    }

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
