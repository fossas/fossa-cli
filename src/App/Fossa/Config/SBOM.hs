module App.Fossa.Config.SBOM (
  mkSubCommand,
  SBOMFile (..),
  SBOMCommand,
  SBOMScanConfig (..),
  SBOMAnalyzeConfig (..),
) where

import App.Fossa.DebugDir (DebugDirRef)
import App.Fossa.Config.Common (
  CommonOpts (..),
 )
import App.Fossa.Config.ConfigFile (
  ConfigFile,
  resolveLocalConfigFile,
 )
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Config.SBOM.Analyze (SBOMAnalyzeConfig, SBOMAnalyzeOptions (..))
import App.Fossa.Config.SBOM.Analyze qualified as Analyze
import App.Fossa.Config.SBOM.Common (SBOMFile (..))
import App.Fossa.Config.SBOM.Test (SBOMTestOptions (..))
import App.Fossa.Config.SBOM.Test qualified as Test
import App.Fossa.Subcommand (EffStack, GetCommonOpts (getCommonOpts), GetSeverity (getSeverity), SubCommand (SubCommand))
import App.Fossa.Test (TestConfig)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Effect.Logger (Logger, Severity (SevDebug, SevInfo))
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Options.Applicative (
  InfoMod,
  Parser,
  progDescDoc,
  subparser,
 )
import Style (formatStringToDoc)

sbomCmdInfo :: InfoMod a
sbomCmdInfo = progDescDoc $ formatStringToDoc "Run an SBOM analysis or test"

mkSubCommand :: (SBOMScanConfig -> EffStack ()) -> SubCommand SBOMCommand SBOMScanConfig
mkSubCommand = SubCommand "sbom" sbomCmdInfo parser loadConfig mergeOpts

mergeOpts ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  ) =>
  DebugDirRef ->
  Maybe ConfigFile ->
  EnvVars ->
  SBOMCommand ->
  m SBOMScanConfig
mergeOpts debugDirRef cfgfile envvars = \case
  SBOMAnalyze opts -> AnalyzeCfg <$> Analyze.mergeOpts debugDirRef cfgfile envvars opts
  SBOMTest opts -> TestCfg <$> Test.mergeOpts cfgfile envvars opts

loadConfig ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  SBOMCommand ->
  m (Maybe ConfigFile)
loadConfig = resolveLocalConfigFile . getCfgFilePath

getCfgFilePath :: SBOMCommand -> Maybe FilePath
getCfgFilePath = \case
  SBOMAnalyze opts -> optConfig $ analyzeCommons opts
  SBOMTest opts -> optConfig $ testCommons opts

data SBOMCommand
  = SBOMAnalyze SBOMAnalyzeOptions
  | SBOMTest SBOMTestOptions

data SBOMScanConfig
  = AnalyzeCfg SBOMAnalyzeConfig
  | TestCfg TestConfig
  deriving (Show, Generic)

instance ToJSON SBOMScanConfig where
  toEncoding = genericToEncoding defaultOptions

instance GetSeverity SBOMCommand where
  getSeverity = \case
    SBOMAnalyze (SBOMAnalyzeOptions{analyzeCommons = CommonOpts{optDebug}}) -> fromBool optDebug
    SBOMTest (SBOMTestOptions{testCommons = CommonOpts{optDebug}}) -> fromBool optDebug
    where
      fromBool b = if b then SevDebug else SevInfo

instance GetCommonOpts SBOMCommand where
  getCommonOpts = \case
    SBOMAnalyze (SBOMAnalyzeOptions{analyzeCommons}) -> Just analyzeCommons
    SBOMTest (SBOMTestOptions{testCommons}) -> Just testCommons

parser :: Parser SBOMCommand
parser = subparser $ Analyze.subcommand SBOMAnalyze <> Test.subcommand SBOMTest
