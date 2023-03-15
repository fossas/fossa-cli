{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.ListTargets (
  mkSubCommand,
  ListTargetsCliOpts,
  ListTargetsConfig (..),
) where

import App.Fossa.Config.Analyze (
  ExperimentalAnalyzeConfig (ExperimentalAnalyzeConfig),
 )
import App.Fossa.Config.Common (
  CommonOpts (..),
  baseDirArg,
  collectBaseDir,
  commonOpts,
 )
import App.Fossa.Config.ConfigFile (
  ConfigFile (configExperimental),
  ExperimentalConfigs (gradle),
  ExperimentalGradleConfigs (gradleConfigsOnly),
  resolveLocalConfigFile,
 )
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Subcommand (EffStack, GetCommonOpts (getCommonOpts), GetSeverity (getSeverity), SubCommand (SubCommand))
import App.Types (BaseDir)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Effect.Logger (Has, Logger, Severity (SevDebug, SevInfo))
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Options.Applicative (InfoMod, Parser, progDesc)

mkSubCommand :: (ListTargetsConfig -> EffStack ()) -> SubCommand ListTargetsCliOpts ListTargetsConfig
mkSubCommand = SubCommand "list-targets" listTargetsInfo parser loadConfig mergeOpts

loadConfig ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Logger sig m
  ) =>
  ListTargetsCliOpts ->
  m (Maybe ConfigFile)
loadConfig = resolveLocalConfigFile . optConfig . commons

listTargetsInfo :: InfoMod a
listTargetsInfo = progDesc "List available analysis-targets in a directory (projects and sub-projects)"

parser :: Parser ListTargetsCliOpts
parser = ListTargetsCliOpts <$> commonOpts <*> baseDirArg

mergeOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  ListTargetsCliOpts ->
  m ListTargetsConfig
mergeOpts cfgfile _envvars ListTargetsCliOpts{..} = do
  let basedir = collectBaseDir cliBaseDir
      experimentalPrefs = collectExperimental cfgfile
  ListTargetsConfig
    <$> basedir
    <*> pure experimentalPrefs

collectExperimental :: Maybe ConfigFile -> ExperimentalAnalyzeConfig
collectExperimental maybeCfg =
  ExperimentalAnalyzeConfig 
    (fmap
      gradleConfigsOnly
      (maybeCfg >>= configExperimental >>= gradle))
    False -- This should be ok because its discovery should not work differently than the old Go modules tactic.

data ListTargetsCliOpts = ListTargetsCliOpts
  { commons :: CommonOpts
  , cliBaseDir :: FilePath
  }

instance GetSeverity ListTargetsCliOpts where
  getSeverity ListTargetsCliOpts{commons = CommonOpts{optDebug}} = if optDebug then SevDebug else SevInfo

instance GetCommonOpts ListTargetsCliOpts where
  getCommonOpts ListTargetsCliOpts{commons} = Just commons

data ListTargetsConfig = ListTargetsConfig
  { baseDir :: BaseDir
  , experimental :: ExperimentalAnalyzeConfig
  }
  deriving (Show, Generic)

instance ToJSON ListTargetsConfig where
  toEncoding = genericToEncoding defaultOptions
