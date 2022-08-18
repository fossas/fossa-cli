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
  resolveLocalConfigFile,
 )
import App.Fossa.Config.Container.Analyze (ContainerAnalyzeConfig, ContainerAnalyzeOptions (..))
import App.Fossa.Config.Container.Analyze qualified as Analyze
import App.Fossa.Config.Container.Common (ImageText (..))
import App.Fossa.Config.Container.Dump (ContainerDumpScanConfig (..), ContainerDumpScanOptions (..))
import App.Fossa.Config.Container.Dump qualified as Dump
import App.Fossa.Config.Container.ListTargets qualified as ListTargets
import App.Fossa.Config.Container.Parse (ContainerParseFileConfig)
import App.Fossa.Config.Container.Parse qualified as Parse
import App.Fossa.Config.Container.Test (ContainerTestConfig, ContainerTestOptions (..), OutputFormat (..))
import App.Fossa.Config.Container.Test qualified as Test
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Subcommand (EffStack, GetCommonOpts (getCommonOpts), GetSeverity (getSeverity), SubCommand (SubCommand))
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Text (Text)
import Effect.Logger (Logger, Severity (SevDebug, SevInfo))
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Options.Applicative (
  InfoMod,
  Parser,
  hsubparser,
  internal,
  progDesc,
  subparser,
  (<|>),
 )

containerCmdInfo :: InfoMod a
containerCmdInfo = progDesc "Run in container-scanning mode"

mkSubCommand :: (ContainerScanConfig -> EffStack ()) -> SubCommand ContainerCommand ContainerScanConfig
mkSubCommand = SubCommand "container" containerCmdInfo parser loadConfig mergeOpts

mergeOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
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
  ContainerListTargets opts -> ListTargetsCfg <$> Analyze.mergeOpts cfgfile envvars opts

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
  cmd -> resolveLocalConfigFile $ getCfgFilePath cmd

getCfgFilePath :: ContainerCommand -> Maybe FilePath
getCfgFilePath = \case
  ContainerAnalyze opts -> optConfig $ analyzeCommons opts
  ContainerTest opts -> optConfig $ testCommons opts
  -- We only use the config file for analyze and test
  _ -> Nothing

data ContainerCommand
  = ContainerAnalyze ContainerAnalyzeOptions
  | ContainerTest ContainerTestOptions
  | ContainerParseFile Text
  | ContainerDumpScan ContainerDumpScanOptions
  | ContainerListTargets ContainerAnalyzeOptions

data ContainerScanConfig
  = AnalyzeCfg ContainerAnalyzeConfig
  | TestCfg ContainerTestConfig
  | DumpCfg ContainerDumpScanConfig
  | ParseCfg ContainerParseFileConfig
  | ListTargetsCfg ContainerAnalyzeConfig
  deriving (Show, Generic)

instance ToJSON ContainerScanConfig where
  toEncoding = genericToEncoding defaultOptions

instance GetSeverity ContainerCommand where
  getSeverity = \case
    ContainerAnalyze (ContainerAnalyzeOptions{analyzeCommons = CommonOpts{optDebug}}) -> fromBool optDebug
    ContainerTest (ContainerTestOptions{testCommons = CommonOpts{optDebug}}) -> fromBool optDebug
    ContainerParseFile _ -> SevInfo
    ContainerDumpScan _ -> SevInfo
    ContainerListTargets _ -> SevInfo
    where
      fromBool b = if b then SevDebug else SevInfo

instance GetCommonOpts ContainerCommand where
  getCommonOpts = \case
    ContainerAnalyze (ContainerAnalyzeOptions{analyzeCommons}) -> Just analyzeCommons
    ContainerTest (ContainerTestOptions{testCommons}) -> Just testCommons
    ContainerParseFile _ -> Nothing
    ContainerDumpScan _ -> Nothing
    ContainerListTargets (ContainerAnalyzeOptions{analyzeCommons}) -> Just analyzeCommons

parser :: Parser ContainerCommand
parser = public <|> private
  where
    public = hsubparser $ Analyze.subcommand ContainerAnalyze <> Test.subcommand ContainerTest <> ListTargets.subcommand ContainerListTargets
    private = subparser $ internal <> Parse.subcommand ContainerParseFile <> Dump.subcommand ContainerDumpScan
