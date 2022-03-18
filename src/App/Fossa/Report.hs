{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Report (
  report,
  reportSubCommand,
) where

import App.Fossa.API.BuildWait (
  waitForIssues,
  waitForScanCompletion,
 )
import App.Fossa.Config.Report (ReportCliOptions, ReportConfig (..), mkSubCommand)
import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.Subcommand (SubCommand)
import App.Types (ProjectRevision (..))
import Control.Carrier.StickyLogger (logSticky, runStickyLogger)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift)
import Control.Timeout (timeout')
import Data.Aeson qualified as Aeson
import Data.String.Conversion (decodeUtf8)
import Data.Text.Extra (showT)
import Effect.Logger (
  Logger,
  Pretty (pretty),
  Severity (SevInfo),
  logInfo,
  logStdout,
 )

reportSubCommand :: SubCommand ReportCliOptions ReportConfig
reportSubCommand = mkSubCommand report

report ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  ReportConfig ->
  m ()
report ReportConfig{..} = do
  -- TODO: refactor this code duplicate from `fossa test`
  {-
  Most of this module (almost everything below this line) has been copied
  from App.Fossa.Test.  I wanted to push this out sooner, and refactoring
  everything right away was not appropriate for the timing of this command.

  * Waiting for builds and issue scans (separately, but also together)
    * Above includes errors, types, and scaffolding
  -}
  runStickyLogger SevInfo $
    timeout' timeoutDuration $ \cancelToken -> do
      logInfo ""
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")

      logSticky "[ Waiting for build completion... ]"

      waitForScanCompletion apiOpts revision cancelToken

      logSticky "[ Waiting for issue scan completion... ]"

      _ <- waitForIssues apiOpts revision cancelToken

      logSticky $ "[ Fetching " <> showT reportType <> " report... ]"

      jsonValue <- Fossa.getAttribution apiOpts revision

      logStdout . decodeUtf8 $ Aeson.encode jsonValue
