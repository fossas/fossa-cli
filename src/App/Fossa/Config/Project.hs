module App.Fossa.Config.Project (
  ProjectCommand (..),
  ProjectConfig (..),
  mkSubCommand,
) where

import App.Fossa.Config.ConfigFile (ConfigFile, resolveLocalConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars (..))
import App.Fossa.Config.Project.Edit as Edit

import App.Fossa.Subcommand (EffStack, GetCommonOpts, GetSeverity (..), SubCommand (..))
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Effect.Lift (Lift)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Effect.Logger (Logger, Severity (..))
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Options.Applicative (InfoMod, Parser, progDescDoc, subparser)
import Style (formatStringToDoc)

projectInfo :: InfoMod a
projectInfo = progDescDoc $ formatStringToDoc "FOSSA project"

mkSubCommand :: (ProjectConfig -> EffStack ()) -> SubCommand ProjectCommand ProjectConfig
mkSubCommand = SubCommand "project" projectInfo projectCliParser loadConfig projectMergeOpts

newtype ProjectCommand = Edit EditOpts

newtype ProjectConfig = EditCfg EditConfig
  deriving (Show, Generic)

instance GetCommonOpts ProjectCommand

instance ToJSON ProjectConfig where
  toEncoding = genericToEncoding defaultOptions

instance GetSeverity ProjectCommand where
  getSeverity :: ProjectCommand -> Severity
  getSeverity (Edit (EditOpts{debug})) = if debug then SevDebug else SevInfo

projectMergeOpts ::
  (Has Diagnostics sig m) =>
  Maybe FilePath ->
  Maybe ConfigFile ->
  EnvVars ->
  ProjectCommand ->
  m ProjectConfig
projectMergeOpts maybeDebugDir cfgFile envVars (Edit opts) = EditCfg <$> Edit.mergeOpts maybeDebugDir cfgFile envVars opts

loadConfig ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  ) =>
  ProjectCommand ->
  m (Maybe ConfigFile)
loadConfig (Edit opts) = resolveLocalConfigFile $ Edit.configOpts opts

projectCliParser :: Parser ProjectCommand
projectCliParser =
  subparser $
    Edit.subcommand Edit
