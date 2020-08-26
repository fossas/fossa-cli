{-# LANGUAGE NumericUnderscores #-}

module App.Fossa.API.BuildWait (
    waitForBuild,
    waitForIssues
) where

import App.Types
import qualified App.Fossa.FossaAPIV1 as Fossa
import Control.Effect.Lift (Lift, sendIO)
import Control.Carrier.Diagnostics
import Control.Concurrent (threadDelay)
import Effect.Logger
import Text.URI (URI)

pollDelaySeconds :: Int
pollDelaySeconds = 8

data WaitError = BuildFailed -- ^ we encountered the FAILED status on a build
  deriving (Eq, Ord, Show)

instance ToDiagnostic WaitError where
  renderDiagnostic BuildFailed = "The build failed. Check the FOSSA webapp for more details."

waitForBuild
  :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m)
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
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForBuild baseurl key revision

waitForIssues
  :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m)
  => URI
  -> ApiKey -- ^ api key
  -> ProjectRevision
  -> m Fossa.Issues
waitForIssues baseurl key revision = do
  issues <- Fossa.getIssues baseurl key revision
  case Fossa.issuesStatus issues of
    "WAITING" -> do
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForIssues baseurl key revision
    _ -> pure issues
