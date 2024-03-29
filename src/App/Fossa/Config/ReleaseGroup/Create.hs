{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.ReleaseGroup.Create (
  CreateConfig (..),
  CreateOpts (..),
  cliParser,
  mergeOpts,
  subcommand,
) where

import App.Fossa.Config.Common (configFileOpt, configHelp)
import App.Fossa.Config.ConfigFile (ConfigFile, ConfigReleaseGroup (..), configReleaseGroup)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Config.ReleaseGroup.Common (ReleaseGroupCommonOpts (..), ReleaseGroupProjectOpts (..), collectApiOpts, extractReleaseGroupConfigValue, mergeReleaseGroupProjectRevision, mergeReleaseGroupRelease, mergeReleaseGroupTitle, releaseGroupCommonOpts, releaseGroupProjectOpts)
import App.Types (ReleaseGroupReleaseRevision (..), ReleaseGroupRevision (..))
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
  (<|>),
 )
import Options.Applicative.Builder (progDescDoc)
import Style (applyFossaStyle, formatStringToDoc, stringToHelpDoc)

releaseGroupCreateInfo :: InfoMod a
releaseGroupCreateInfo = progDescDoc $ formatStringToDoc "Create a FOSSA release group"

subcommand :: (CreateOpts -> a) -> Mod CommandFields a
subcommand f = command "create" $ info (f <$> cliParser) releaseGroupCreateInfo

data CreateConfig = CreateConfig
  { apiOpts :: ApiOpts
  , releaseGroupRevision :: ReleaseGroupRevision
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON CreateConfig where
  toEncoding = genericToEncoding defaultOptions

data CreateOpts = CreateOpts
  { releaseGroupCommon :: ReleaseGroupCommonOpts
  , configOpts :: Maybe FilePath
  , titleOpts :: Maybe Text
  , releaseOpts :: Maybe Text
  , projectsOpts :: Maybe [ReleaseGroupProjectOpts]
  , licensePolicyOpts :: Maybe Text
  , securityPolicyOpts :: Maybe Text
  , qualityPolicyOpts :: Maybe Text
  , teamsOpts :: Maybe [Text]
  }
  deriving (Eq, Ord, Show, Generic)

cliParser :: Parser CreateOpts
cliParser =
  CreateOpts
    <$> releaseGroupCommonOpts
    <*> configFileOpt
    <*> optional (strOption (applyFossaStyle <> long "title" <> short 't' <> stringToHelpDoc "The title of the FOSSA release group"))
    <*> optional (strOption (applyFossaStyle <> long "release" <> short 'r' <> stringToHelpDoc "The release of the FOSSA release group"))
    <*> optional (some (releaseGroupProjectOpts))
    <*> optional (strOption (applyFossaStyle <> long "license-policy" <> short 'l' <> stringToHelpDoc "The name of the license compliance policy you want to assign to the FOSSA release group"))
    <*> optional (strOption (applyFossaStyle <> long "security-policy" <> short 's' <> stringToHelpDoc "The name of the security policy you want to assign to the FOSSA release group"))
    <*> optional (strOption (applyFossaStyle <> long "quality-policy" <> short 'q' <> stringToHelpDoc "The name of the quality policy you want to assign to the FOSSA release group"))
    <*> optional (some (strOption (applyFossaStyle <> long "team" <> short 'T' <> stringToHelpDoc "The team you want to assign to the FOSSA release group")))

-- <*> optional (some (strOption (applyFossaStyle <> long "team" <> short 'T' <> stringToHelpDoc "The team you want to assign to the FOSSA release group")))
mergeOpts ::
  ( Has Diagnostics sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  CreateOpts ->
  m CreateConfig
mergeOpts maybeConfig envVars cliOpts@CreateOpts{..} = do
  apiOpts <- collectApiOpts maybeConfig envVars releaseGroupCommon
  releaseGroupRevision <- collectReleaseGroupRevision maybeConfig cliOpts
  pure $ CreateConfig apiOpts releaseGroupRevision

collectReleaseGroupRevision :: (Has Diagnostics sig m) => Maybe ConfigFile -> CreateOpts -> m ReleaseGroupRevision
collectReleaseGroupRevision maybeConfig CreateOpts{..} = do
  let licensePolicy = licensePolicyOpts <|> extractReleaseGroupConfigValue maybeConfig configReleaseGroupLicensePolicy
      securityPolicy = securityPolicyOpts <|> extractReleaseGroupConfigValue maybeConfig configReleaseGroupSecurityPolicy
      qualityPolicy = qualityPolicyOpts <|> extractReleaseGroupConfigValue maybeConfig configReleaseGroupQualityPolicy
      teams = teamsOpts <|> extractReleaseGroupConfigValue maybeConfig configReleaseGroupTeams

  title <- mergeReleaseGroupTitle titleOpts $ extractReleaseGroupConfigValue maybeConfig configReleaseGroupTitle
  releaseTitle <- mergeReleaseGroupRelease releaseOpts $ extractReleaseGroupConfigValue maybeConfig configReleaseGroupRelease
  projects <- mergeReleaseGroupProjectRevision projectsOpts $ extractReleaseGroupConfigValue maybeConfig configReleaseGroupProjects

  let releaseRevision = ReleaseGroupReleaseRevision releaseTitle projects

  pure $
    ReleaseGroupRevision
      { releaseGroupTitle = title
      , releaseGroupReleaseRevision = releaseRevision
      , releaseGroupLicensePolicy = licensePolicy
      , releaseGroupSecurityPolicy = securityPolicy
      , releaseGroupQualityPolicy = qualityPolicy
      , releaseGroupTeams = teams
      }
