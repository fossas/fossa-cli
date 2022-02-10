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
  resolveConfigFile,
 )
import App.Fossa.Subcommand (EffStack, GetSeverity (getSeverity), SubCommand (SubCommand))
import App.Types (BaseDir)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift, sendIO)
import Effect.Logger (Has, Logger, Severity (SevDebug, SevInfo))
import Effect.ReadFS (ReadFS)
import Options.Applicative (InfoMod, Parser, progDesc)
import Path.IO (getCurrentDir)

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
loadConfig ListTargetsCliOpts{..} = do
  curdir <- sendIO getCurrentDir
  resolveConfigFile curdir $ optConfig commons

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
  p ->
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
  ExperimentalAnalyzeConfig $
    fmap
      gradleConfigsOnly
      (maybeCfg >>= configExperimental >>= gradle)

data ListTargetsCliOpts = ListTargetsCliOpts
  { commons :: CommonOpts
  , cliBaseDir :: FilePath
  }

instance GetSeverity ListTargetsCliOpts where
  getSeverity ListTargetsCliOpts{commons = CommonOpts{optDebug}} = if optDebug then SevDebug else SevInfo

data ListTargetsConfig = ListTargetsConfig
  { baseDir :: BaseDir
  , experimental :: ExperimentalAnalyzeConfig
  }
  deriving (Show)
