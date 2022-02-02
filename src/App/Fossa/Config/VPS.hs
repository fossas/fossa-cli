{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.VPS (
  mkSubCommand,
  VPSCliOpts,
  VPSConfig (..),
  AnalyzeConfig (..),
  TestConfig (..),
  AOSPNoticeConfig (..),
  ReportConfig (..),
  OutputFormat (..),
  FollowSymlinks (..),
  LicenseOnlyScan (..),
  SkipIPRScan (..),
  ReportType (..),
) where

import App.Fossa.Config.Common (
  CommonOpts (..),
 )
import App.Fossa.Config.ConfigFile (
  ConfigFile,
  resolveConfigFile,
 )
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Config.VPS.AOSP (AOSPNoticeConfig, AOSPNoticeOpts (AOSPNoticeOpts))
import App.Fossa.Config.VPS.AOSP qualified as AOSP
import App.Fossa.Config.VPS.Analyze (AnalyzeConfig, AnalyzeOpts (AnalyzeOpts), FollowSymlinks (..), LicenseOnlyScan (..), SkipIPRScan (..))
import App.Fossa.Config.VPS.Analyze qualified as Analyze
import App.Fossa.Config.VPS.Report (ReportConfig, ReportOpts (ReportOpts), ReportType)
import App.Fossa.Config.VPS.Report qualified as Report
import App.Fossa.Config.VPS.Test (OutputFormat, TestConfig, TestOpts (TestOpts))
import App.Fossa.Config.VPS.Test qualified as Test
import App.Fossa.Subcommand (
  EffStack,
  GetSeverity (..),
  SubCommand (SubCommand),
 )
import Control.Effect.Diagnostics (
  Diagnostics,
 )
import Control.Effect.Lift (Has, Lift, sendIO)
import Effect.Exec (Exec)
import Effect.Logger (Logger, Severity (SevDebug, SevInfo))
import Effect.ReadFS (ReadFS)
import Options.Applicative (
  InfoMod,
  Parser,
  hsubparser,
  progDesc,
 )
import Path.IO (getCurrentDir)

mkSubCommand :: (VPSConfig -> EffStack ()) -> SubCommand VPSCliOpts VPSConfig
mkSubCommand = SubCommand "vps" vpsInfo cliParser loadConfig mergeOpts

vpsInfo :: InfoMod a
vpsInfo = progDesc "Run in Vendored Package Scan mode"

loadConfig ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  VPSCliOpts ->
  m (Maybe ConfigFile)
loadConfig opts = do
  curdir <- sendIO getCurrentDir
  resolveConfigFile curdir . optConfig $ getCommons opts

mergeOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  VPSCliOpts ->
  m (VPSConfig)
mergeOpts cfgfile envvars = \case
  AnalyzeCommand opts -> AnalyzeCfg <$> Analyze.mergeOpts cfgfile envvars opts
  AOSPNoticeCommand opts -> AOSPNoticeCfg <$> AOSP.mergeOpts cfgfile envvars opts
  TestCommand opts -> TestCfg <$> Test.mergeOpts cfgfile envvars opts
  ReportCommand opts -> ReportCfg <$> Report.mergeOpts cfgfile envvars opts

getCommons :: VPSCliOpts -> CommonOpts
getCommons = \case
  AnalyzeCommand AnalyzeOpts{..} -> analyzeCliCommons
  AOSPNoticeCommand AOSPNoticeOpts{..} -> aospCommons
  TestCommand TestOpts{..} -> testCommons
  ReportCommand ReportOpts{..} -> reportCommons

instance GetSeverity VPSCliOpts where
  getSeverity opts = if debugMode then SevDebug else SevInfo
    where
      debugMode = optDebug $ getCommons opts

data VPSConfig
  = AnalyzeCfg AnalyzeConfig
  | AOSPNoticeCfg AOSPNoticeConfig
  | TestCfg TestConfig
  | ReportCfg ReportConfig
  deriving (Eq, Ord, Show)

data VPSCliOpts
  = AnalyzeCommand AnalyzeOpts
  | AOSPNoticeCommand AOSPNoticeOpts
  | TestCommand TestOpts
  | ReportCommand ReportOpts
  deriving (Eq, Ord, Show)

cliParser :: Parser VPSCliOpts
cliParser =
  hsubparser
    ( Analyze.subcommand AnalyzeCommand
        <> Test.subcommand TestCommand
        <> Report.subcommand ReportCommand
        <> AOSP.subcommand AOSPNoticeCommand
    )
