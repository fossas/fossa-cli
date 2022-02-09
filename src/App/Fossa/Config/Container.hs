module App.Fossa.Config.Container (
  mkSubCommand,
  ImageText (..),
  OutputFormat (..),
  ContainerCommand,
  ContainerScanConfig (..),
  ContainerAnalyzeConfig (..),
  ContainerTestConfig (..),
  ContainerDumpScanConfig (..),
  ContainerParseFileConfig (..),
) where

import App.Fossa.Config.Common (
  CommonOpts (..),
 )
import App.Fossa.Config.ConfigFile (
  ConfigFile,
  resolveConfigFile,
 )
import App.Fossa.Config.Container.Analyze (ContainerAnalyzeConfig, ContainerAnalyzeOptions (..))
import App.Fossa.Config.Container.Analyze qualified as Analyze
import App.Fossa.Config.Container.Common (ImageText (..))
import App.Fossa.Config.Container.Dump (ContainerDumpScanConfig (..), ContainerDumpScanOptions (..))
import App.Fossa.Config.Container.Dump qualified as Dump
import App.Fossa.Config.Container.Parse (ContainerParseFileConfig)
import App.Fossa.Config.Container.Parse qualified as Parse
import App.Fossa.Config.Container.Test (ContainerTestConfig, ContainerTestOptions (..), OutputFormat (..))
import App.Fossa.Config.Container.Test qualified as Test
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Subcommand (EffStack, GetSeverity (getSeverity), SubCommand (SubCommand))
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift, sendIO)
import Effect.Logger (Logger, Severity (SevDebug, SevInfo))
import Effect.ReadFS (ReadFS)
import Options.Applicative (
  InfoMod,
  Parser,
  hsubparser,
  internal,
  progDesc,
  subparser,
  (<|>),
 )
import Path.IO (getCurrentDir)

containerCmdInfo :: InfoMod a
containerCmdInfo = progDesc "Run in container-scanning mode"

mkSubCommand :: (ContainerScanConfig -> EffStack ()) -> SubCommand ContainerCommand ContainerScanConfig
mkSubCommand = SubCommand "container" containerCmdInfo parser loadConfig mergeOpts

mergeOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  ContainerCommand ->
  m ContainerScanConfig
mergeOpts cfgfile envvars = \case
  ContainerAnalyze opts -> AnalyzeCfg <$> Analyze.mergeOpts cfgfile envvars opts
  ContainerTest opts -> TestCfg <$> Test.mergeOpts cfgfile envvars opts
  ContainerParseFile fp -> ParseCfg <$> Parse.mergeOpts cfgfile envvars fp
  ContainerDumpScan opts -> DumpCfg <$> Dump.mergeOpts cfgfile envvars opts

loadConfig ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  ContainerCommand ->
  m (Maybe ConfigFile)
loadConfig = \case
  ContainerParseFile _ -> pure Nothing
  ContainerDumpScan _ -> pure Nothing
  -- Only parse config file if we're running analyze or test
  cmd -> do
    curdir <- sendIO getCurrentDir
    resolveConfigFile curdir $ getCfgFilePath cmd

getCfgFilePath :: ContainerCommand -> Maybe FilePath
getCfgFilePath = \case
  ContainerAnalyze opts -> optConfig $ analyzeCommons opts
  ContainerTest opts -> optConfig $ testCommons opts
  -- We only use the config file for analyze and test
  _ -> Nothing

data ContainerCommand
  = ContainerAnalyze ContainerAnalyzeOptions
  | ContainerTest ContainerTestOptions
  | ContainerParseFile FilePath
  | ContainerDumpScan ContainerDumpScanOptions

data ContainerScanConfig
  = AnalyzeCfg ContainerAnalyzeConfig
  | TestCfg ContainerTestConfig
  | DumpCfg ContainerDumpScanConfig
  | ParseCfg ContainerParseFileConfig
  deriving (Show)

instance GetSeverity ContainerCommand where
  getSeverity = \case
    ContainerAnalyze (ContainerAnalyzeOptions{analyzeCommons = CommonOpts{optDebug}}) -> fromBool optDebug
    ContainerTest (ContainerTestOptions{testCommons = CommonOpts{optDebug}}) -> fromBool optDebug
    ContainerParseFile _ -> SevInfo
    ContainerDumpScan _ -> SevInfo
    where
      fromBool b = if b then SevDebug else SevInfo

parser :: Parser ContainerCommand
parser = public <|> private
  where
    public = hsubparser $ Analyze.subcommand ContainerAnalyze <> Test.subcommand ContainerTest
    private = subparser $ internal <> Parse.subcommand ContainerParseFile <> Dump.subcommand ContainerDumpScan
