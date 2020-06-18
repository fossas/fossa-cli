
module App.Fossa.Test
  ( testMain
  , TestOutputType(..)
  ) where

import Prologue
import qualified Prelude as Unsafe

import qualified App.Fossa.FossaAPIV1 as Fossa
import App.Fossa.ProjectInference
import Control.Carrier.Diagnostics
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.IO (hPutStrLn)
import Effect.Logger
import Path.IO
import System.IO (stderr)
import System.Exit (exitSuccess, exitFailure)
import Text.URI (URI)
import qualified Data.Aeson as Aeson
import Data.Text.Lazy.Encoding (decodeUtf8)

pollDelaySeconds :: Int
pollDelaySeconds = 8

data TestOutputType
  = TestOutputPretty -- ^ pretty output format for issues
  | TestOutputJson -- ^ use json output for issues

testMain
  :: URI -- ^ api base url
  -> Text -- ^ api key
  -> Severity
  -> Int -- ^ timeout (seconds)
  -> TestOutputType
  -> Maybe Text -- ^ cli override for name
  -> Maybe Text -- ^ cli override for revision
  -> IO ()
testMain baseurl apiKey logSeverity timeoutSeconds outputType overrideName overrideRevision = do
  basedir <- getCurrentDir

  void $ timeout timeoutSeconds $ withLogger logSeverity $ do
    result <- runDiagnostics $ do
      inferred <- inferProject basedir
 
      let projectName = fromMaybe (inferredName inferred) overrideName
          projectRevision = fromMaybe (inferredRevision inferred) overrideRevision

      logInfo ""
      logInfo ("Using project name: `" <> pretty projectName <> "`")
      logInfo ("Using revision: `" <> pretty projectRevision <> "`")

      logSticky "[ Waiting for build completion... ]"

      waitForBuild baseurl apiKey projectName projectRevision

      logSticky "[ Waiting for issue scan completion... ]"
      issues <- waitForIssues baseurl apiKey projectName projectRevision
      logSticky ""

      if null (Fossa.issuesIssues issues)
        then logInfo "Test passed! 0 issues found"
        else do
          case outputType of
            TestOutputPretty -> logError (renderedIssues issues)
            TestOutputJson -> logStdout . pretty . decodeUtf8 . Aeson.encode $ issues
          liftIO exitFailure

    case result of
      Left failure -> do
        logError $ renderFailureBundle failure
        liftIO exitFailure
      Right _ -> liftIO exitSuccess

  -- we call exitSuccess/exitFailure in each branch above. the only way we get
  -- here is if we time out
  hPutStrLn stderr "Timed out while waiting for issues scan"
  exitFailure

renderedIssues :: Fossa.Issues -> Doc ann
renderedIssues issues = rendered
  where
    padding :: Int
    padding = 20

    issuesList :: [Fossa.Issue]
    issuesList = Fossa.issuesIssues issues

    categorize :: Ord k => (v -> k) -> [v] -> Map k [v]
    categorize f = M.fromListWith (++) . map (\v -> (f v, [v]))

    issuesByType :: Map Fossa.IssueType [Fossa.Issue]
    issuesByType = categorize Fossa.issueType issuesList

    renderSection :: Fossa.IssueType -> [Fossa.Issue] -> Doc ann
    renderSection issueType rawIssues =
      renderHeader issueType <> line <> vsep (map renderIssue rawIssues) <> line

    rendered :: Doc ann
    rendered = vsep
      [renderSection issueType rawIssues | (issueType,rawIssues) <- M.toList issuesByType]

    renderHeader :: Fossa.IssueType -> Doc ann
    renderHeader ty = vsep
      [ "========================================================================"
      , pretty $ Fossa.renderIssueType ty
      , "========================================================================"
      , hsep $ map (fill padding) $ case ty of
          Fossa.IssuePolicyConflict -> ["Dependency", "Revision", "License"]
          Fossa.IssuePolicyFlag -> ["Dependency", "Revision", "License"]
          _ -> ["Dependency", "Revision"]
      , ""
      ]

    renderIssue :: Fossa.Issue -> Doc ann
    renderIssue issue = hsep (map format [name, revision, license])
      where
        format :: Text -> Doc ann
        format = fill padding . pretty

        locatorSplit = T.split (\c -> c == '$' || c == '+') (Fossa.issueRevisionId issue)

        name = fromMaybe (Fossa.issueRevisionId issue) (locatorSplit !? 1)
        revision = fromMaybe "" (locatorSplit !? 2)
        license = fromMaybe "" (Fossa.ruleLicenseId =<< Fossa.issueRule issue)

        (!?) :: [a] -> Int -> Maybe a
        xs !? ix
          | length xs <= ix = Nothing
          | otherwise = Just (xs Unsafe.!! ix)

data TestError = TestBuildFailed -- ^ we encountered the FAILED status on a build
  deriving (Show, Generic)

instance ToDiagnostic TestError where
  renderDiagnostic TestBuildFailed = "The build failed. Check the FOSSA webapp for more details."

waitForBuild
  :: (Has Diagnostics sig m, MonadIO m, Has Logger sig m)
  => URI
  -> Text -- ^ api key
  -> Text -- ^ project name
  -> Text -- ^ project revision
  -> m ()
waitForBuild baseurl key projectName projectRevision = do
  build <- Fossa.getLatestBuild baseurl key projectName projectRevision
 
  case Fossa.buildTaskStatus (Fossa.buildTask build) of
    Fossa.StatusSucceeded -> pure ()
    Fossa.StatusFailed -> fatal TestBuildFailed
    otherStatus -> do
      logSticky $ "[ Waiting for build completion... last status: " <> viaShow otherStatus <> " ]"
      liftIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForBuild baseurl key projectName projectRevision

waitForIssues
  :: (Has Diagnostics sig m, MonadIO m, Has Logger sig m)
  => URI
  -> Text -- ^ api key
  -> Text -- ^ project name
  -> Text -- ^ project revision
  -> m Fossa.Issues
waitForIssues baseurl key projectName projectRevision = do
  issues <- Fossa.getIssues baseurl key projectName projectRevision
  case Fossa.issuesStatus issues of
    "WAITING" -> do
      liftIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForIssues baseurl key projectName projectRevision
    _ -> pure issues

timeout
  :: Int -- ^ number of seconds before timeout
  -> IO a
  -> IO (Maybe a)
timeout seconds act = either id id <$> Async.race (Just <$> act) (threadDelay (seconds * 1_000_000) *> pure Nothing)
