{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.ReleaseGroup.Create (
  CreateConfig (..),
  CreateOpts (..),
  cliParser,
  mergeOpts,
  subcommand,
) where

import App.Fossa.Config.Common (configHelp)
import App.Fossa.Config.ConfigFile (ConfigFile, ConfigReleaseGroup (..), configReleaseGroup)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Config.ReleaseGroup.Common (ReleaseGroupCommonOpts (..), ReleaseGroupProjectOpts (..), collectApiOpts, mergeReleaseGroupProjectRevision, mergeReleaseGroupRelease, mergeReleaseGroupTitle, releaseGroupCommonOpts, releaseGroupProjectOpts)
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
  many,
  optional,
  short,
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
    <*> optional (strOption (applyFossaStyle <> long "config" <> short 'c' <> helpDoc configHelp))
    <*> optional (strOption (applyFossaStyle <> long "title" <> short 't' <> stringToHelpDoc "The title of the FOSSA release group"))
    <*> optional (strOption (applyFossaStyle <> long "release" <> short 'r' <> stringToHelpDoc "The release of the FOSSA release group"))
    <*> optional (many (releaseGroupProjectOpts))
    <*> optional (strOption (applyFossaStyle <> long "license-policy" <> short 'l' <> stringToHelpDoc "The name of the license compliance policy you want to assign to the FOSSA release group"))
    <*> optional (strOption (applyFossaStyle <> long "security-policy" <> short 's' <> stringToHelpDoc "The name of the security policy you want to assign to the FOSSA release group"))
    <*> optional (strOption (applyFossaStyle <> long "quality-policy" <> short 'q' <> stringToHelpDoc "The name of the quality policy you want to assign to the FOSSA release group"))
    <*> optional (many (strOption (applyFossaStyle <> long "team" <> short 'T' <> stringToHelpDoc "The team you want to assign to the FOSSA release group")))

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
  let licensePolicy = licensePolicyOpts <|> (maybeConfig >>= configReleaseGroup >>= configReleaseGroupLicensePolicy)
      securityPolicy = securityPolicyOpts <|> (maybeConfig >>= configReleaseGroup >>= configReleaseGroupSecurityPolicy)
      qualityPolicy = securityPolicyOpts <|> (maybeConfig >>= configReleaseGroup >>= configReleaseGroupQualityPolicy)
      -- NOTE: teamsOpts and projectsOpts default to Just [] when it is not set through CLI flags.
      --       Convert these to Nothing so we can try to extract from the config file.
      teams = case teamsOpts of
        Nothing -> (maybeConfig >>= configReleaseGroup >>= configReleaseGroupTeams)
        Just [] -> (maybeConfig >>= configReleaseGroup >>= configReleaseGroupTeams)
        Just teamsOpts' -> Just teamsOpts'
      projectsOpts' = case projectsOpts of
        Nothing -> Nothing
        Just [] -> Nothing
        Just projects -> Just projects

  title <- mergeReleaseGroupTitle titleOpts (maybeConfig >>= configReleaseGroup >>= configReleaseGroupTitle)
  releaseTitle <- mergeReleaseGroupRelease releaseOpts (maybeConfig >>= configReleaseGroup >>= configReleaseGroupRelease)
  projects <- mergeReleaseGroupProjectRevision projectsOpts' (maybeConfig >>= configReleaseGroup >>= configReleaseGroupProjects)

  let releaseRevision = ReleaseGroupReleaseRevision releaseTitle projects

  pure $ ReleaseGroupRevision title releaseRevision licensePolicy securityPolicy qualityPolicy teams
