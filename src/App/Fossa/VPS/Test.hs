module App.Fossa.VPS.Test (
  testMain,
  TestOutputType (..),
) where

import App.Fossa.API.BuildWait
import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.ProjectInference
import App.Fossa.VPS.Scan.Core qualified as VPSCore
import App.Fossa.VPS.Scan.ScotlandYard qualified as ScotlandYard
import App.Types
import Control.Carrier.Diagnostics hiding (fromMaybe)
import Control.Carrier.Stack (runStack)
import Control.Carrier.StickyLogger (logSticky, runStickyLogger)
import Control.Effect.Lift (sendIO)
import Data.Aeson qualified as Aeson
import Data.String.Conversion
import Data.Text.IO (hPutStrLn)
import Effect.Exec (runExecIO)
import Effect.Logger
import Effect.ReadFS
import Fossa.API.Types (ApiOpts, Issues (..))
import System.Exit (exitFailure)
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
testMain (BaseDir basedir) apiOpts logSeverity timeoutSeconds outputType override = do
  _ <- timeout timeoutSeconds . withDefaultLogger logSeverity . runStickyLogger SevInfo $
    runStack . logWithExit_ . runReadFSIO . runExecIO $ do
      revision <- mergeOverride override <$> (inferProjectFromVCS basedir <||> inferProjectCached basedir <||> inferProjectDefault basedir)

      logInfo ""
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")

      logSticky "[ Getting latest scan ID ]"

      Fossa.Organization orgId _ <- Fossa.getOrganization apiOpts
      let locator = VPSCore.createLocator (projectName revision) orgId

      scan <- ScotlandYard.getLatestScan apiOpts locator (projectRevision revision)

      logSticky "[ Waiting for component scan... ]"

      waitForSherlockScan apiOpts locator (ScotlandYard.responseScanId scan)

      logSticky "[ Waiting for issue scan completion... ]"
      issues <- waitForIssues apiOpts revision
      logSticky ""

      case issuesCount issues of
        0 -> do
          logInfo "Test passed! 0 issues found"
          case outputType of
            TestOutputJson -> logStdout . decodeUtf8 . Aeson.encode $ issues
            TestOutputPretty -> pure ()
        n -> do
          logError $ "Test failed. Number of issues found: " <> pretty n
          if null (issuesIssues issues)
            then logError "Check the webapp for more details, or use a full-access API key (currently using a push-only API key)"
            else case outputType of
              TestOutputPretty -> pure ()
              TestOutputJson -> logStdout . decodeUtf8 . Aeson.encode $ issues
          sendIO exitFailure

  -- we call exitSuccess/exitFailure in each branch above. the only way we get
  -- here is if we time out
  hPutStrLn stderr "Timed out while waiting for issues scan"
  exitFailure
