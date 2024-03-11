module App.Fossa.Config.ReleaseGroup (
  ReleaseGroupCommand,
  ReleaseGroupConfig (..),
  mkSubCommand,
) where

import App.Fossa.Config.ConfigFile (ConfigFile, resolveLocalConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars (..))
import App.Fossa.Config.ReleaseGroup.AddProjects as AddProjects
import App.Fossa.Config.ReleaseGroup.Common (ReleaseGroupCommonOpts (..))
import App.Fossa.Config.ReleaseGroup.Create as Create
import App.Fossa.Config.ReleaseGroup.Delete as Delete
import App.Fossa.Config.ReleaseGroup.DeleteRelease as DeleteRelease
import App.Fossa.Subcommand (EffStack, GetCommonOpts, GetSeverity (..), SubCommand (..))
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Effect.Lift (Lift)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Effect.Logger (Logger, Severity (..))
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Options.Applicative (InfoMod, Parser, progDescDoc, subparser)
import Style (formatStringToDoc)

releaseGroupInfo :: InfoMod a
releaseGroupInfo = progDescDoc $ formatStringToDoc "FOSSA release group"

mkSubCommand :: (ReleaseGroupConfig -> EffStack ()) -> SubCommand ReleaseGroupCommand ReleaseGroupConfig
mkSubCommand = SubCommand "release-group" releaseGroupInfo releaseGroupCliParser loadConfig releaseGroupMergeOpts

data ReleaseGroupCommand
  = AddProjects AddProjectsOpts
  | Create CreateOpts
  | Delete DeleteOpts
  | DeleteRelease DeleteReleaseOpts

data ReleaseGroupConfig
  = AddProjectsCfg AddProjectsConfig
  | CreateCfg CreateConfig
  | DeleteCfg DeleteConfig
  | DeleteReleaseCfg DeleteReleaseConfig
  deriving (Show, Generic)

instance GetCommonOpts ReleaseGroupCommand

instance ToJSON ReleaseGroupConfig where
  toEncoding = genericToEncoding defaultOptions

instance GetSeverity ReleaseGroupCommand where
  getSeverity :: ReleaseGroupCommand -> Severity
  getSeverity = \case
    AddProjects (AddProjectsOpts{AddProjects.releaseGroupCommon = ReleaseGroupCommonOpts{debug}}) -> if debug then SevDebug else SevInfo
    Create (CreateOpts{Create.releaseGroupCommon = ReleaseGroupCommonOpts{debug}}) -> if debug then SevDebug else SevInfo
    Delete (DeleteOpts{Delete.releaseGroupCommon = ReleaseGroupCommonOpts{debug}}) -> if debug then SevDebug else SevInfo
    DeleteRelease (DeleteReleaseOpts{DeleteRelease.releaseGroupCommon = ReleaseGroupCommonOpts{debug}}) -> if debug then SevDebug else SevInfo

releaseGroupMergeOpts ::
  ( Has Diagnostics sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  ReleaseGroupCommand ->
  m ReleaseGroupConfig
releaseGroupMergeOpts cfg envvars = \case
  AddProjects opts -> AddProjectsCfg <$> AddProjects.mergeOpts cfg envvars opts
  Create opts -> CreateCfg <$> Create.mergeOpts cfg envvars opts
  Delete opts -> DeleteCfg <$> Delete.mergeOpts cfg envvars opts
  DeleteRelease opts -> DeleteReleaseCfg <$> DeleteRelease.mergeOpts cfg envvars opts

loadConfig ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  ) =>
  ReleaseGroupCommand ->
  m (Maybe ConfigFile)
loadConfig = \case
  AddProjects opts -> resolveLocalConfigFile $ AddProjects.configOpts opts
  Create opts -> resolveLocalConfigFile $ Create.configOpts opts
  Delete _ -> pure Nothing
  DeleteRelease _ -> pure Nothing

releaseGroupCliParser :: Parser ReleaseGroupCommand
releaseGroupCliParser =
  subparser $
    AddProjects.subcommand AddProjects
      <> Create.subcommand Create
      <> Delete.subcommand Delete
      <> DeleteRelease.subcommand DeleteRelease
