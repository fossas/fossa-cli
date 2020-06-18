module App.Fossa.Report
  ( reportMain
  , ReportType (..)
  ) where

import Prologue

import qualified App.Fossa.FossaAPIV1 as Fossa
import App.Fossa.ProjectInference
import Control.Concurrent (threadDelay)
import Control.Carrier.Diagnostics
import qualified Control.Concurrent.Async as Async
import Data.Text.IO (hPutStrLn)
import Effect.Logger
import Path.IO
import System.IO (stderr)
import System.Exit (exitSuccess, exitFailure)
import Text.URI (URI)
import Data.Text.Lazy.Encoding (decodeUtf8)

data ReportType =
    AttributionReport

reportName :: ReportType -> Text
reportName r = case r of
  AttributionReport -> "attribution"

reportMain ::
  URI -- ^ api base url
  -> Text -- ^ api key
  -> Severity
  -> Int -- ^ timeout (seconds)
  -> ReportType
  -> Maybe Text -- ^ cli override for name
  -> Maybe Text -- ^ cli override for revision
  -> IO ()
reportMain baseUri apiKey logSeverity timeoutSeconds reportType overrideName overrideRevision = do
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
    result <- runDiagnostics $ do
      inferred <- inferProject basedir
      let project = fromMaybe (inferredName inferred) overrideName
          revision = fromMaybe (inferredRevision inferred) overrideRevision

      logInfo ""
      logInfo ("Using project name: `" <> pretty project <> "`")
      logInfo ("Using revision: `" <> pretty revision <> "`")

      logSticky "[ Waiting for build completion... ]"

      waitForBuild baseUri apiKey project revision

      logSticky "[ Waiting for issue scan completion... ]"
      _ <- waitForIssues baseUri apiKey project revision
      logSticky ""

      logSticky $ "[ Fetching " <> pretty (reportName reportType) <> " report... ]"
      jsonValue <- case reportType of
        AttributionReport ->
          Fossa.getAttribution baseUri apiKey project revision
      logSticky ""
        
      logStdout . pretty . decodeUtf8 $ encode jsonValue

    case result of
      Left err -> do
        logError $ renderFailureBundle err
        liftIO exitFailure
      Right _ -> liftIO exitSuccess

  hPutStrLn stderr "Timed out while waiting for build/issues scan"
  exitFailure

waitForBuild
  :: (Has Diagnostics sig m, MonadIO m, Has Logger sig m)
  => URI
  -> Text -- ^ api key
  -> Text -- ^ project name
  -> Text -- ^ project revision
  -> m ()
waitForBuild baseUri key project revision = do
  build <- Fossa.getLatestBuild baseUri key project revision
  case Fossa.buildTaskStatus (Fossa.buildTask build) of
    Fossa.StatusSucceeded -> pure ()
    Fossa.StatusFailed -> fatal BuildFailed
    otherStatus -> do
      logSticky $ "[ Waiting for build completion... last status: " <> viaShow otherStatus <> " ]"
      liftIO $ threadDelay (pollDelaySeconds * 1000000)
      waitForBuild baseUri key project revision

waitForIssues
  :: (Has Diagnostics sig m, MonadIO m, Has Logger sig m)
  => URI
  -> Text -- ^ api key
  -> Text -- ^ project name
  -> Text -- ^ project revision
  -> m ()
waitForIssues baseUri key project revision = do
  issues <- Fossa.getIssues baseUri key project revision
  case Fossa.issuesStatus issues of
    "WAITING" -> do
      liftIO $ threadDelay (pollDelaySeconds * 1000000)
      waitForIssues baseUri key project revision
    _ -> pure ()

pollDelaySeconds :: Int
pollDelaySeconds = 8

timeout
  :: Int -- ^ number of seconds before timeout
  -> IO a
  -> IO (Maybe a)
timeout seconds act = either id id <$> Async.race (Just <$> act) (threadDelay (seconds * 1000000) *> pure Nothing)

data WaitError
  = BuildFailed -- ^ we encountered the FAILED status on a build
  deriving (Show, Generic)

instance ToDiagnostic WaitError where
  renderDiagnostic BuildFailed = "The build failed. Check the FOSSA webapp for more details."
