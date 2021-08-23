module App.Fossa.Test (
  testMain,
  TestOutputType (..),
) where

import App.Fossa.API.BuildWait (
  timeout,
  waitForIssues,
  waitForScanCompletion,
 )
import App.Fossa.ProjectInference (
  inferProjectCached,
  inferProjectDefault,
  inferProjectFromVCS,
  mergeOverride,
 )
import App.Types (
  BaseDir (BaseDir),
  OverrideProject,
  ProjectRevision (projectName, projectRevision),
 )
import Control.Carrier.Diagnostics (logWithExit_, (<||>))
import Control.Carrier.StickyLogger (logSticky, runStickyLogger)
import Control.Effect.Lift (sendIO)
import Data.Aeson qualified as Aeson
import Data.Functor (void)
import Data.String.Conversion (decodeUtf8)
import Data.Text.IO (hPutStrLn)
import Effect.Logger (
  Severity (SevInfo),
  logError,
  logInfo,
  logStdout,
  pretty,
  withDefaultLogger,
 )
import Effect.ReadFS (runReadFSIO)
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
  void . timeout timeoutSeconds . withDefaultLogger logSeverity . runStickyLogger SevInfo $
    logWithExit_ . runReadFSIO $ do
      revision <- mergeOverride override <$> (inferProjectFromVCS basedir <||> inferProjectCached basedir <||> inferProjectDefault basedir)

      logInfo ""
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")

      logSticky "[ Waiting for build completion... ]"

      waitForScanCompletion apiOpts revision

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
            else case outputType of
              TestOutputPretty -> logError $ pretty issues
              TestOutputJson -> logStdout . decodeUtf8 . Aeson.encode $ issues

          sendIO exitFailure

  -- we call exitSuccess/exitFailure in each branch above. the only way we get
  -- here is if we time out
  hPutStrLn stderr "Timed out while waiting for issues scan"
  exitFailure
