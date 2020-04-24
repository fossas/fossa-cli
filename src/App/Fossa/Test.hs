
module App.Fossa.Test
  ( testMain
  ) where

import Prologue
import qualified Prelude as Unsafe

import qualified App.Fossa.FossaAPIV1 as Fossa
import App.Fossa.ProjectInference
import Control.Carrier.Error.Either
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.IO (hPutStrLn)
import Effect.Logger
import Network.HTTP.Req
import Path.IO
import System.IO (stderr)
import System.Exit (exitSuccess, exitFailure)

pollDelaySeconds :: Int
pollDelaySeconds = 8

testMain
  :: Url 'Https -- ^ api base url
  -> Text -- ^ api key
  -> Severity
  -> Int -- ^ timeout (seconds)
  -> Maybe Text -- ^ cli override for name
  -> Maybe Text -- ^ cli override for revision
  -> IO ()
testMain baseurl apiKey logSeverity timeoutSeconds overrideName overrideRevision= do
  basedir <- getCurrentDir

  void $ timeout timeoutSeconds $ withLogger logSeverity $ do
    result <- runError @TestError $ do
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
      issues <- waitForIssues baseurl apiKey revision
      logSticky ""

      if null (Fossa.issuesIssues issues)
        then logInfo "Test passed! 0 issues found"
        else do
          logError (renderedIssues issues)
          liftIO exitFailure

    case result of
      Left err -> do
        logError $ pretty (renderTestError err)
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

data TestError
  = TestErrorAPI Fossa.FossaError -- ^ we encountered an API request error
  | TestBuildFailed -- ^ we encountered the FAILED status on a build
  deriving (Show, Generic)

renderTestError :: TestError -> Text
renderTestError (TestErrorAPI err) = "An API error occurred: " <> T.pack (show err)
renderTestError TestBuildFailed = "The build failed. Check the FOSSA webapp for more details."

waitForBuild
  :: (Has (Error TestError) sig m, MonadIO m, Has Logger sig m)
  => Url 'Https
  -> Text -- ^ api key
  -> Fossa.ProjectRevision
  -> m ()
waitForBuild baseurl key revision = do
  maybeBuild <- Fossa.fossaReq $ Fossa.getLatestBuild baseurl key revision
  case maybeBuild of
    Left err -> throwError (TestErrorAPI err)
    Right build -> do
      case Fossa.buildTaskStatus (Fossa.buildTask build) of
        Fossa.StatusSucceeded -> pure ()
        Fossa.StatusFailed -> throwError TestBuildFailed
        otherStatus -> do
          logSticky $ "[ Waiting for build completion... last status: " <> viaShow otherStatus <> " ]"
          liftIO $ threadDelay (pollDelaySeconds * 1_000_000)
          waitForBuild baseurl key revision

waitForIssues
  :: (Has (Error TestError) sig m, MonadIO m, Has Logger sig m)
  => Url 'Https
  -> Text -- ^ api key
  -> Fossa.ProjectRevision
  -> m Fossa.Issues
waitForIssues baseurl key revision = do
  result <- Fossa.fossaReq $ Fossa.getIssues baseurl key revision
  case result of
    Left err -> throwError (TestErrorAPI err)
    Right issues ->
      case Fossa.issuesStatus issues of
        "WAITING" -> do
          liftIO $ threadDelay (pollDelaySeconds * 1_000_000)
          waitForIssues baseurl key revision
        _ -> pure issues

timeout
  :: Int -- ^ number of seconds before timeout
  -> IO a
  -> IO (Maybe a)
timeout seconds act = either id id <$> Async.race (Just <$> act) (threadDelay (seconds * 1_000_000) *> pure Nothing)
