module App.Fossa.Config.Container (
  mkSubCommand,
  ImageText (..),
  OutputFormat (..),
  ContainerCommand,
  ContainerScanConfig (..),
  ContainerAnalyzeConfig (..),
  ContainerTestConfig (..),
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
import App.Fossa.Config.Container.ListTargets (ContainerListTargetsConfig, ContainerListTargetsOptions)
import App.Fossa.Config.Container.ListTargets qualified as ListTargets
import App.Fossa.Config.Container.Test (ContainerTestConfig, ContainerTestOptions (..), OutputFormat (..))
import App.Fossa.Config.Container.Test qualified as Test
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Subcommand (EffStack, GetCommonOpts (getCommonOpts), GetSeverity (getSeverity), SubCommand (SubCommand))
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Effect.Logger (Logger, Severity (SevDebug, SevInfo))
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Options.Applicative (
  InfoMod,
  Parser,
  hsubparser,
  progDesc,
 )

containerCmdInfo :: InfoMod a
containerCmdInfo = progDesc "Run in container-scanning mode"

mkSubCommand :: (ContainerScanConfig -> EffStack ()) -> SubCommand ContainerCommand ContainerScanConfig
mkSubCommand = SubCommand "container" containerCmdInfo parser loadConfig mergeOpts

mergeOpts ::
  ( Has Diagnostics sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  ContainerCommand ->
  m ContainerScanConfig
mergeOpts cfgfile envvars = \case
  ContainerAnalyze opts -> AnalyzeCfg <$> Analyze.mergeOpts cfgfile envvars opts
  ContainerTest opts -> TestCfg <$> Test.mergeOpts cfgfile envvars opts
  ContainerListTargets opts -> ListTargetsCfg <$> ListTargets.mergeOpts cfgfile envvars opts

loadConfig ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  ContainerCommand ->
  m (Maybe ConfigFile)
loadConfig = \case
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
  | ContainerListTargets ContainerListTargetsOptions

data ContainerScanConfig
  = AnalyzeCfg ContainerAnalyzeConfig
  | TestCfg ContainerTestConfig
  | ListTargetsCfg ContainerListTargetsConfig
  deriving (Show, Generic)

instance ToJSON ContainerScanConfig where
  toEncoding = genericToEncoding defaultOptions

instance GetSeverity ContainerCommand where
  getSeverity = \case
    ContainerAnalyze (ContainerAnalyzeOptions{analyzeCommons = CommonOpts{optDebug}}) -> fromBool optDebug
    ContainerTest (ContainerTestOptions{testCommons = CommonOpts{optDebug}}) -> fromBool optDebug
    ContainerListTargets _ -> SevInfo
    where
      fromBool b = if b then SevDebug else SevInfo

instance GetCommonOpts ContainerCommand where
  getCommonOpts = \case
    ContainerAnalyze (ContainerAnalyzeOptions{analyzeCommons}) -> Just analyzeCommons
    ContainerTest (ContainerTestOptions{testCommons}) -> Just testCommons
    ContainerListTargets _ -> Nothing

parser :: Parser ContainerCommand
parser = public
  where
    public = hsubparser $ Analyze.subcommand ContainerAnalyze <> Test.subcommand ContainerTest <> ListTargets.subcommand ContainerListTargets
