module App.Fossa.Test
  ( testMain
  , TestOutputType(..)
  ) where

import App.Fossa.API.BuildWait
import App.Fossa.ProjectInference
import App.Types
import Control.Carrier.Diagnostics hiding (fromMaybe)
import Control.Carrier.StickyLogger (runStickyLogger, logSticky)
import Control.Effect.Lift (sendIO)
import Data.Aeson qualified as Aeson
import Data.Functor (void)
import Data.String.Conversion (decodeUtf8)
import Data.Text.IO (hPutStrLn)
import Effect.Logger
import Effect.ReadFS
import Fossa.API.Types (ApiOpts, Issues (..))
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)

data TestOutputType
  = TestOutputPretty -- ^ pretty output format for issues
  | TestOutputJson -- ^ use json output for issues

testMain
  :: BaseDir
  -> ApiOpts
  -> Severity
  -> Int -- ^ timeout (seconds)
  -> TestOutputType
  -> OverrideProject
  -> IO ()
testMain (BaseDir basedir) apiOpts logSeverity timeoutSeconds outputType override = do
  void . timeout timeoutSeconds . withDefaultLogger logSeverity . runStickyLogger $ do
    result <- runDiagnostics . runReadFSIO $ do
      revision <- mergeOverride override <$> (inferProjectFromVCS basedir <||> inferProjectCached basedir <||> inferProjectDefault basedir)

      logInfo ""
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")

      logSticky "[ Waiting for build completion... ]"

      waitForBuild apiOpts revision

      logSticky "[ Waiting for issue scan completion... ]"
      issues <- waitForIssues apiOpts revision
      logSticky ""
      logInfo ""

      case issuesCount issues of
        0 -> logInfo "Test passed! 0 issues found"
        n -> do
          logError $ "Test failed. Number of issues found: " <> pretty n
          if null (issuesIssues issues)
            then logError "Check the webapp for more details, or use a full-access API key (currently using a push-only API key)"
            else
              case outputType of
                TestOutputPretty -> logError $ pretty issues
                TestOutputJson -> logStdout . decodeUtf8 . Aeson.encode $ issues

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
