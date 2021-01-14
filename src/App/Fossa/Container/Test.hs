{-# LANGUAGE BlockArguments #-}

module App.Fossa.Container.Test
  ( TestOutputType (..),
    testMain,
  )
where

import App.Fossa.API.BuildWait
import App.Fossa.Container
import App.Types (OverrideProject (..), ProjectRevision (..))
import Control.Carrier.Diagnostics
import Control.Effect.Lift
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import Data.Functor (void)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.IO (hPutStrLn)
import Effect.Logger
import Fossa.API.Types (ApiOpts (..), Issues (..))
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)

data TestOutputType
  = -- | pretty output format for issues
    TestOutputPretty
  | -- | json format issues
    TestOutputJson

testMain ::
  ApiOpts ->
  Severity ->
  -- | Timeout, in seconds
  Int ->
  TestOutputType ->
  OverrideProject ->
  ImageText ->
  IO ()
testMain apiOpts logSeverity timeoutSeconds outputType override image = do
  void $ timeout timeoutSeconds $ withLogger logSeverity $ do
    result <- runDiagnostics $ testInner apiOpts outputType override image
    case result of
      Left err -> do
        logError $ renderFailureBundle err
        sendIO exitFailure
      Right (ResultBundle _ _) -> sendIO exitSuccess

  hPutStrLn stderr "Timed out while wait for issues"
  exitFailure

testInner ::
  (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m, MonadIO m) =>
  ApiOpts ->
  TestOutputType ->
  OverrideProject ->
  ImageText ->
  m ()
testInner apiOpts outputType override image = do
  logDebug "Running embedded syft binary"
  
  containerScan <- runSyft image >>= toContainerScan
  let revision = extractRevision override containerScan

  logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
  logInfo ("Using project revision: `" <> pretty (projectRevision revision) <> "`")

  logSticky "[ Waiting for build completion ]"
  waitForBuild apiOpts revision

  logSticky "[ Waiting for issue scan completion ]"
  issues <- waitForIssues apiOpts revision
  logSticky ""

  case issuesCount issues of
    0 -> logInfo "Test passed! 0 issues found"
    n -> do
      logError $ "Test failed. Number of issues found: " <> pretty n
      if null (issuesIssues issues)
        then logError "Check webapp for more details, or use a full-access API key (currently using a push-only API key)"
        else do
          case outputType of
            TestOutputPretty -> logError $ pretty issues
            TestOutputJson -> logStdout . pretty . decodeUtf8 . Aeson.encode $ issues
          sendIO exitFailure
