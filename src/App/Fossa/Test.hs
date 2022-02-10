{-# LANGUAGE BlockArguments #-}

module App.Fossa.Test (
  testSubCommand,
) where

import App.Fossa.API.BuildWait (
  waitForIssues,
  waitForScanCompletion,
 )
import App.Fossa.Config.Test (OutputFormat (TestOutputJson, TestOutputPretty), TestCliOpts, TestConfig)
import App.Fossa.Config.Test qualified as Config
import App.Fossa.Subcommand (SubCommand)
import App.Types (
  ProjectRevision (projectName, projectRevision),
 )
import Control.Algebra (Has)
import Control.Carrier.StickyLogger (logSticky, runStickyLogger)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.Lift (Lift)
import Control.Timeout (timeout')
import Data.Aeson qualified as Aeson
import Data.String.Conversion (decodeUtf8)
import Data.Text.Extra (showT)
import Effect.Logger (
  Logger,
  Severity (SevInfo),
  logError,
  logInfo,
  logStdout,
  pretty,
 )
import Fossa.API.Types (Issues (..))

testSubCommand :: SubCommand TestCliOpts TestConfig
testSubCommand = Config.mkSubCommand testMain

testMain ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  TestConfig ->
  m ()
testMain config = runStickyLogger SevInfo $
  timeout' (Config.timeout config) $
    \cancelFlag -> do
      let apiOpts = Config.apiOpts config
          revision = Config.projectRevision config
          outputType = Config.outputFormat config
      logInfo ""
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")

      logSticky "[ Waiting for build completion... ]"

      waitForScanCompletion apiOpts revision cancelFlag

      logSticky "[ Waiting for issue scan completion... ]"
      issues <- waitForIssues apiOpts revision cancelFlag
      logSticky ""
      logInfo ""

      case issuesCount issues of
        0 -> do
          logInfo "Test passed! 0 issues found"
          case outputType of
            TestOutputJson -> logStdout . decodeUtf8 . Aeson.encode $ issues
            TestOutputPretty -> pure ()
        n -> do
          if null (issuesIssues issues)
            then logError "A push-only API key was used, so issue details cannot be displayed. Check the webapp for issue details, or rerun this command with a full-access API key."
            else case outputType of
              TestOutputPretty -> logError $ pretty issues
              TestOutputJson -> logStdout . decodeUtf8 . Aeson.encode $ issues
          fatalText $ "The scan has revealed issues. Number of issues found: " <> showT n
