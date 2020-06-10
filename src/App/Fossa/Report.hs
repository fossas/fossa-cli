module App.Fossa.Report
  ( reportMain
  , ReportType (..)
  ) where

import Prologue

import qualified App.Fossa.FossaAPIV1 as Fossa
import App.Fossa.ProjectInference
import Control.Carrier.Error.Either
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Data.Text as T
import Data.Text.IO (hPutStrLn)
import Effect.Logger
import Path.IO
import System.IO (stderr)
import System.Exit (exitSuccess, exitFailure)
import OptionExtensions
import Data.Text.Lazy.Encoding (decodeUtf8)

data ReportType =
    AttributionReport

reportName :: ReportType -> Text
reportName r = case r of
  AttributionReport -> "attribution"

getReport
  :: (Has (Error WaitError) sig m, MonadIO m)
  => (UrlOption -> Text -> Fossa.ProjectRevision -> Fossa.FossaReq a)
  -> UrlOption
  -> Text -- ^ api key
  -> Fossa.ProjectRevision
  -> m a
getReport f baseurl key revision = do
  maybeValue <- Fossa.fossaReq $ f baseurl key revision
  case maybeValue of
    Left err -> throwError (APIError err)
    Right jsonValue -> pure jsonValue

reportMain :: 
  UrlOption -- ^ api base url
  -> Text -- ^ api key
  -> Severity
  -> Int -- ^ timeout (seconds)
  -> ReportType
  -> Maybe Text -- ^ cli override for name
  -> Maybe Text -- ^ cli override for revision
  -> IO ()
reportMain baseurl apiKey logSeverity timeoutSeconds reportType overrideName overrideRevision = do
  basedir <- getCurrentDir

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
  
  void $ timeout timeoutSeconds $ withLogger logSeverity $ do
    result <- runError @WaitError $ do
      inferred <- inferProject basedir
      let revision = Fossa.ProjectRevision
            (fromMaybe (inferredName inferred) overrideName)
            (fromMaybe (inferredRevision inferred) overrideRevision)

      logInfo ""
      logInfo ("Using project name: `" <> pretty (Fossa.projectName revision) <> "`")
      logInfo ("Using revision: `" <> pretty (Fossa.projectRevision revision) <> "`")

      logSticky "[ Waiting for build completion... ]"

      waitForBuild baseurl apiKey revision

      logSticky "[ Waiting for issue scan completion... ]"
      _ <- waitForIssues baseurl apiKey revision
      logSticky ""

      logSticky $ "[ Fetching " <> pretty (reportName reportType) <> " report... ]"
      jsonValue <- case reportType of
        AttributionReport ->
          getReport Fossa.getAttribution baseurl apiKey revision
      logSticky ""
        
      logStdout . pretty . decodeUtf8 $ encode jsonValue

    case result of
      Left err -> do
        logError $ pretty (renderWaitError err)
        liftIO exitFailure
      Right _ -> liftIO exitSuccess

  hPutStrLn stderr "Timed out while waiting for build/issues scan"
  exitFailure

waitForBuild
  :: (Has (Error WaitError) sig m, MonadIO m, Has Logger sig m)
  => UrlOption
  -> Text -- ^ api key
  -> Fossa.ProjectRevision
  -> m ()
waitForBuild baseurl key revision = do
  maybeBuild <- Fossa.fossaReq $ Fossa.getLatestBuild baseurl key revision
  case maybeBuild of
    Left err -> throwError (APIError err)
    Right build ->
      case Fossa.buildTaskStatus (Fossa.buildTask build) of
        Fossa.StatusSucceeded -> pure ()
        Fossa.StatusFailed -> throwError BuildFailed
        otherStatus -> do
          logSticky $ "[ Waiting for build completion... last status: " <> viaShow otherStatus <> " ]"
          liftIO $ threadDelay (pollDelaySeconds * 1000000)
          waitForBuild baseurl key revision

waitForIssues
  :: (Has (Error WaitError) sig m, MonadIO m, Has Logger sig m)
  => UrlOption
  -> Text -- ^ api key
  -> Fossa.ProjectRevision
  -> m ()
waitForIssues baseurl key revision = do
  result <- Fossa.fossaReq $ Fossa.getIssues baseurl key revision
  case result of
    Left err -> throwError (APIError err)
    Right issues ->
      case Fossa.issuesStatus issues of
        "WAITING" -> do
          liftIO $ threadDelay (pollDelaySeconds * 1000000)
          waitForIssues baseurl key revision
        _ -> pure ()

renderWaitError :: WaitError -> Text
renderWaitError (APIError err) = "An API error occurred: " <> T.pack (show err)
renderWaitError BuildFailed = "The build failed. Check the FOSSA webapp for more details."

pollDelaySeconds :: Int
pollDelaySeconds = 8

timeout
  :: Int -- ^ number of seconds before timeout
  -> IO a
  -> IO (Maybe a)
timeout seconds act = either id id <$> Async.race (Just <$> act) (threadDelay (seconds * 1000000) *> pure Nothing)

data WaitError
  = APIError Fossa.FossaError -- ^ we encountered an API request error
  | BuildFailed -- ^ we encountered the FAILED status on a build
  deriving (Show, Generic)
