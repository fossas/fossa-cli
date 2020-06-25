module App.Fossa.API.BuildWait (
    waitForBuild,
    waitForIssues
) where

import Prologue

import App.Fossa.CliTypes
import qualified App.Fossa.FossaAPIV1 as Fossa
import Control.Carrier.Diagnostics
import Control.Concurrent (threadDelay)
import Effect.Logger
import Text.URI (URI)

pollDelaySeconds :: Int
pollDelaySeconds = 8

data WaitError = BuildFailed -- ^ we encountered the FAILED status on a build
  deriving (Show, Generic)

instance ToDiagnostic WaitError where
  renderDiagnostic BuildFailed = "The build failed. Check the FOSSA webapp for more details."

waitForBuild
  :: (Has Diagnostics sig m, MonadIO m, Has Logger sig m)
  => URI
  -> ApiKey -- ^ api key
  -> ProjectRevision
  -> m ()
waitForBuild baseurl key revision = do
  build <- Fossa.getLatestBuild baseurl key revision
 
  case Fossa.buildTaskStatus (Fossa.buildTask build) of
    Fossa.StatusSucceeded -> pure ()
    Fossa.StatusFailed -> fatal BuildFailed
    otherStatus -> do
      logSticky $ "[ Waiting for build completion... last status: " <> viaShow otherStatus <> " ]"
      liftIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForBuild baseurl key revision

waitForIssues
  :: (Has Diagnostics sig m, MonadIO m, Has Logger sig m)
  => URI
  -> ApiKey -- ^ api key
  -> ProjectRevision
  -> m Fossa.Issues
waitForIssues baseurl key revision = do
  issues <- Fossa.getIssues baseurl key revision
  case Fossa.issuesStatus issues of
    "WAITING" -> do
      liftIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForIssues baseurl key revision
    _ -> pure issues