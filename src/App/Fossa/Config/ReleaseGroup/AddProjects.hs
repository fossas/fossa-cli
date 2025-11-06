{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.ReleaseGroup.AddProjects (
  AddProjectsOpts (..),
  AddProjectsConfig (..),
  subcommand,
  cliParser,
  mergeOpts,
) where

import App.Fossa.DebugDir (DebugDirRef)
import App.Fossa.Config.Common (configFileOpt)
import App.Fossa.Config.ConfigFile (ConfigFile, configReleaseGroup, configReleaseGroupProjects, configReleaseGroupRelease, configReleaseGroupTitle)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Config.ReleaseGroup.Common (ReleaseGroupCommonOpts (..), ReleaseGroupProjectOpts (..), collectApiOpts, extractReleaseGroupConfigValue, mergeReleaseGroupProjectRevision, mergeReleaseGroupRelease, mergeReleaseGroupTitle, releaseGroupCommonOpts, releaseGroupProjectOpts, releaseGroupReleaseTitleOpts, releaseGroupTitleOpts)
import App.Types (ReleaseGroupReleaseRevision (..))
import Control.Effect.Diagnostics (Diagnostics, Has)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Data.Text (Text)
import Fossa.API.Types (ApiOpts)
import GHC.Generics (Generic)
import Options.Applicative (
  CommandFields,
  InfoMod,
  Mod,
  Parser,
  command,
  info,
  optional,
  some,
 )
import Options.Applicative.Builder (progDescDoc)
import Style (formatStringToDoc)

releaseGroupAddProjectsInfo :: InfoMod a
releaseGroupAddProjectsInfo = progDescDoc $ formatStringToDoc "Add FOSSA projects a FOSSA release group"

subcommand :: (AddProjectsOpts -> a) -> Mod CommandFields a
subcommand f = command "add-projects" $ info (f <$> cliParser) releaseGroupAddProjectsInfo

data AddProjectsConfig = AddProjectsConfig
  { apiOpts :: ApiOpts
  , title :: Text
  , releaseGroupReleaseRevision :: ReleaseGroupReleaseRevision
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON AddProjectsConfig where
  toEncoding = genericToEncoding defaultOptions

data AddProjectsOpts = AddProjectsOpts
  { releaseGroupCommon :: ReleaseGroupCommonOpts
  , configOpts :: Maybe FilePath
  , titleOpts :: Maybe Text
  , releaseOpts :: Maybe Text
  , projectsOpts :: Maybe [ReleaseGroupProjectOpts]
  }
  deriving (Eq, Ord, Show, Generic)

cliParser :: Parser AddProjectsOpts
cliParser =
  AddProjectsOpts
    <$> releaseGroupCommonOpts
    <*> configFileOpt
    <*> optional releaseGroupTitleOpts
    <*> optional releaseGroupReleaseTitleOpts
    <*> optional (some (releaseGroupProjectOpts))

mergeOpts ::
  (Has Diagnostics sig m) =>
  DebugDirRef ->
  Maybe ConfigFile ->
  EnvVars ->
  AddProjectsOpts ->
  m AddProjectsConfig
mergeOpts _ maybeConfig envVars cliOpts@AddProjectsOpts{..} = do
  apiOpts <- collectApiOpts maybeConfig envVars releaseGroupCommon
  title <- mergeReleaseGroupTitle titleOpts $ extractReleaseGroupConfigValue maybeConfig configReleaseGroupTitle
  releaseGroupReleaseRevision <- collectReleaseGroupReleaseRevision maybeConfig cliOpts
  pure $ AddProjectsConfig apiOpts title releaseGroupReleaseRevision

collectReleaseGroupReleaseRevision :: (Has Diagnostics sig m) => Maybe ConfigFile -> AddProjectsOpts -> m ReleaseGroupReleaseRevision
collectReleaseGroupReleaseRevision maybeConfig AddProjectsOpts{..} = do
  releaseTitle <- mergeReleaseGroupRelease releaseOpts (maybeConfig >>= configReleaseGroup >>= configReleaseGroupRelease)
  projects <- mergeReleaseGroupProjectRevision projectsOpts (maybeConfig >>= configReleaseGroup >>= configReleaseGroupProjects)

  pure $ ReleaseGroupReleaseRevision releaseTitle projects
