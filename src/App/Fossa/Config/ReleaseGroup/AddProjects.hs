{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.ReleaseGroup.AddProjects (
  AddProjectsOpts (..),
  AddProjectsConfig (..),
  subcommand,
  cliParser,
  mergeOpts,
) where

import App.Fossa.Config.Common (configFileOpt, configHelp)
import App.Fossa.Config.ConfigFile (ConfigFile, configReleaseGroup, configReleaseGroupProjects, configReleaseGroupRelease, configReleaseGroupTitle)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Config.ReleaseGroup.Common (ReleaseGroupCommonOpts (..), ReleaseGroupProjectOpts (..), collectApiOpts, extractReleaseGroupConfigValue, mergeReleaseGroupProjectRevision, mergeReleaseGroupRelease, mergeReleaseGroupTitle, releaseGroupCommonOpts, releaseGroupProjectOpts)
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
  helpDoc,
  info,
  long,
  optional,
  short,
  some,
  strOption,
 )
import Options.Applicative.Builder (progDescDoc)
import Style (applyFossaStyle, formatStringToDoc, stringToHelpDoc)

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
    <*> optional (strOption (applyFossaStyle <> long "title" <> short 't' <> stringToHelpDoc "The title of the FOSSA release group"))
    <*> optional (strOption (applyFossaStyle <> long "release" <> short 'r' <> stringToHelpDoc "The release of the FOSSA release group"))
    <*> optional (some (releaseGroupProjectOpts))

mergeOpts ::
  ( Has Diagnostics sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  AddProjectsOpts ->
  m AddProjectsConfig
mergeOpts maybeConfig envVars cliOpts@AddProjectsOpts{..} = do
  apiOpts <- collectApiOpts maybeConfig envVars releaseGroupCommon
  title <- mergeReleaseGroupTitle titleOpts $ extractReleaseGroupConfigValue maybeConfig configReleaseGroupTitle
  releaseGroupReleaseRevision <- collectReleaseGroupReleaseRevision maybeConfig cliOpts
  pure $ AddProjectsConfig apiOpts title releaseGroupReleaseRevision

collectReleaseGroupReleaseRevision :: (Has Diagnostics sig m) => Maybe ConfigFile -> AddProjectsOpts -> m ReleaseGroupReleaseRevision
collectReleaseGroupReleaseRevision maybeConfig AddProjectsOpts{..} = do
  releaseTitle <- mergeReleaseGroupRelease releaseOpts (maybeConfig >>= configReleaseGroup >>= configReleaseGroupRelease)
  projects <- mergeReleaseGroupProjectRevision projectsOpts (maybeConfig >>= configReleaseGroup >>= configReleaseGroupProjects)

  pure $ ReleaseGroupReleaseRevision releaseTitle projects
