{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module App.Fossa.Test (
  testSubCommand,
) where

import App.Fossa.API.BuildWait (
  waitForIssues,
  waitForScanCompletion,
 )
import App.Fossa.Config.Test (
  DiffRevision (..),
  TestCliOpts,
  TestConfig (diffRevision),
  TestOutputFormat (TestOutputJson, TestOutputPretty),
 )
import App.Fossa.Config.Test qualified as Config
import App.Fossa.PreflightChecks (preflightChecks)
import App.Fossa.Subcommand (SubCommand)
import App.Types (
  ProjectRevision (projectName, projectRevision),
 )
import Control.Algebra (Has)
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.StickyLogger (logSticky, runStickyLogger)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.Lift (Lift)
import Control.Timeout (timeout')
import Data.Aeson qualified as Aeson
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text)
import Data.Text.Extra (showT)
import Effect.Logger (
  Logger,
  Severity (SevInfo),
  logError,
  logInfo,
  logStdout,
  pretty,
  vsep,
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
testMain config = do
  _ <- ignoreDebug $ runFossaApiClient (Config.apiOpts config) preflightChecks

  runStickyLogger SevInfo
    . ignoreDebug -- Ignore the debug effect because we don't generate a bundle.
    . runFossaApiClient (Config.apiOpts config)
    . timeout' (Config.timeout config)
    $ \cancelFlag -> do
      let revision = Config.projectRevision config
          outputType = Config.outputFormat config
          diffRev = diffRevision config

      logInfo ""
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")

      case diffRev of
        Nothing -> logInfo ""
        Just (DiffRevision rev) -> do
          logInfo ("diffing against revision: `" <> pretty rev <> "`")
          logSticky $ "[ Checking build completion for " <> rev <> "... ]"
          waitForScanCompletion revision{projectRevision = rev} cancelFlag

      logSticky $ "[ Checking build completion for " <> revision.projectRevision <> "... ]"
      waitForScanCompletion revision cancelFlag

      logSticky "[ Waiting for issue scan completion... ]"
      issues <- waitForIssues revision diffRev cancelFlag
      logSticky ""
      logInfo ""

      case issuesCount issues of
        0 -> do
          logInfo . pretty $ successMsg diffRev
          case outputType of
            TestOutputPretty -> pure ()
            TestOutputJson -> renderJson issues
        n -> do
          if null (issuesIssues issues)
            then
              logError $
                vsep
                  [ "A push-only API key was used, so issue details cannot be displayed."
                  , "Check the webapp for issue details, or rerun this command with a full-access API key."
                  ]
            else case outputType of
              TestOutputPretty -> logError $ pretty issues
              TestOutputJson -> renderJson issues
          fatalText $ issuesFoundMsg diffRev n
  where
    successMsg :: Maybe DiffRevision -> Text
    successMsg diffRevision = case diffRevision of
      Nothing -> "Test passed! 0 issues found"
      Just (DiffRevision rev) -> "Test passed! No new issues found compared to revision: " <> rev <> "."

    issuesFoundMsg :: Maybe DiffRevision -> Int -> Text
    issuesFoundMsg diffRevision n = case diffRevision of
      Nothing -> "The scan has revealed issues. Number of issues found: " <> showT n
      Just (DiffRevision rev) ->
        "The scan has revealed new issues compared to revision: "
          <> rev
          <> ". Number of new issues found: "
          <> showT n

    renderJson :: (Has (Lift IO) sig m, Has Logger sig m) => Issues -> m ()
    renderJson = logStdout . decodeUtf8 . Aeson.encode
