module App.Fossa.VPS.Test
  ( testMain,
    TestOutputType (..),
  )
where

import App.Fossa.API.BuildWait
import qualified App.Fossa.FossaAPIV1 as Fossa
import App.Fossa.ProjectInference
import qualified App.Fossa.VPS.Scan.Core as VPSCore
import qualified App.Fossa.VPS.Scan.ScotlandYard as ScotlandYard
import App.Types
import Control.Carrier.Diagnostics hiding (fromMaybe)
import Control.Effect.Lift (sendIO)
import qualified Data.Aeson as Aeson
import Data.Text.IO (hPutStrLn)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Fossa.API.Types (ApiOpts, Issues (..))
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)

data TestOutputType
  = -- | pretty output format for issues
    TestOutputPretty
  | -- | use json output for issues
    TestOutputJson

testMain ::
  BaseDir ->
  ApiOpts ->
  Severity ->
  -- | timeout (seconds)
  Int ->
  TestOutputType ->
  OverrideProject ->
  IO ()
testMain basedir apiOpts logSeverity timeoutSeconds outputType override = do
  _ <- timeout timeoutSeconds . withLogger logSeverity . runExecIO $ do
    result <- runDiagnostics . runReadFSIO $ do
      override' <- updateOverrideRevision override <$> readCachedRevision 
      revision <- mergeOverride override' <$> inferProject (unBaseDir basedir)

      logInfo ""
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")

      logSticky "[ Getting latest scan ID ]"

      Fossa.Organization orgId <- Fossa.getOrganization apiOpts
      let locator = VPSCore.createLocator (projectName revision) orgId

      scan <- ScotlandYard.getLatestScan apiOpts locator (projectRevision revision)

      logSticky "[ Waiting for component scan... ]"

      waitForSherlockScan apiOpts locator (ScotlandYard.responseScanId scan)

      logSticky "[ Waiting for issue scan completion... ]"
      issues <- waitForIssues apiOpts revision
      logSticky ""

      case issuesCount issues of
        0 -> logInfo "Test passed! 0 issues found"
        n -> do
          logError $ "Test failed. Number of issues found: " <> pretty n
          if null (issuesIssues issues)
            then logError "Check the webapp for more details, or use a full-access API key (currently using a push-only API key)"
            else
              case outputType of
                TestOutputPretty -> pure ()
                TestOutputJson -> logStdout . pretty . decodeUtf8 . Aeson.encode $ issues
          sendIO exitFailure

    case result of
      Left failure -> do
        logError $ renderFailureBundle failure
        sendIO exitFailure
      Right _ -> sendIO exitSuccess

  -- we call exitSuccess/exitFailure in each branch above. the only way we get
  -- here is if we time out
  hPutStrLn stderr "Timed out while waiting for issues scan"
  exitFailure
