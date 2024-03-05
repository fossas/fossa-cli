{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.ReleaseGroup (
  ReleaseGroupCommand,
  ReleaseGroupConfig (..),
  CreateConfig,
  mkSubCommand,
) where

import App.Fossa.Config.Common (CommonOpts, configHelp, endpointHelp, fossaApiKeyCmdText, fossaApiKeyHelp)
import App.Fossa.Config.ConfigFile (ConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Config.Snippets (SnippetsCommand)
import App.Fossa.Subcommand (EffStack, GetCommonOpts, GetSeverity (..), SubCommand (..))
import App.OptionExtensions (uriOption)
import App.Types (BaseDir)
import Control.Carrier.Lift (sendIO)
import Control.Effect.Diagnostics (Diagnostics, Has)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Data.Text (Text)
import Effect.Logger (Logger, Severity (..), logDebug, pretty)
import GHC.Generics (Generic)
import Options.Applicative (CommandFields, InfoMod, Mod, Parser, command, eitherReader, helpDoc, info, long, many, metavar, option, optional, progDesc, progDescDoc, short, strOption, subparser, switch, (<|>))
import Style (applyFossaStyle, formatStringToDoc, stringToHelpDoc)
import Text.Pretty.Simple (pShow)
import Text.URI (URI, mkURI)

releaseGroupInfo :: InfoMod a
releaseGroupInfo = progDescDoc $ formatStringToDoc "FOSSA release group"

releaseGroupCreateInfo :: InfoMod a
releaseGroupCreateInfo = progDescDoc $ formatStringToDoc "Create a FOSSA release group"

mkSubCommand :: (ReleaseGroupConfig -> EffStack ()) -> SubCommand ReleaseGroupCommand ReleaseGroupConfig
mkSubCommand = SubCommand "release-group" releaseGroupInfo cliParser noLoadConfig mergeOpts
  where
    noLoadConfig = const $ pure Nothing

newtype ReleaseGroupCommand
  = ReleaseGroupCreate ReleaseGroupCreateOpts

newtype ReleaseGroupConfig = Create CreateConfig
  deriving (Show, Generic)

instance ToJSON ReleaseGroupConfig where
  toEncoding = genericToEncoding defaultOptions

-- \| Delete DeleteConfig
-- \| DeleteRelease DeleteReleaseConfig

data CreateConfig = CreateConfig
  { severity :: Severity
  , title :: Text
  , release :: Text
  , releaseGroupProjects :: [ReleaseGroupProject]
  , licenseCompliancePolicy :: Maybe Text
  , vulnerabilityManagementPolicy :: Maybe Text
  , teams :: Maybe [Text]
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON CreateConfig where
  toEncoding = genericToEncoding defaultOptions

data ReleaseGroupCommonOpts = ReleaseGroupCommonOpts
  { debug :: Bool
  , baseUrl :: Maybe URI
  , apiKey :: Maybe Text
  , config :: Maybe FilePath
  }
  deriving (Eq, Ord, Show)

data ReleaseGroupCreateOpts = ReleaseGroupCreateOpts
  { releaseGroupCommon :: ReleaseGroupCommonOpts
  , titleOpts :: Text
  , releaseOpts :: Text
  , projectsOpts :: [ReleaseGroupProject]
  , licenseCompliancePolicyOpts :: Maybe Text
  , vulnerabilityManagementPolicyOpts :: Maybe Text
  , teamsOpts :: Maybe [Text]
  }
  deriving (Eq, Ord, Show, Generic)

instance GetSeverity ReleaseGroupCommand where
  getSeverity :: ReleaseGroupCommand -> Severity
  getSeverity = \case
    ReleaseGroupCreate (ReleaseGroupCreateOpts{releaseGroupCommon = ReleaseGroupCommonOpts{debug}}) -> if debug then SevDebug else SevInfo

instance GetSeverity ReleaseGroupCreateOpts where
  getSeverity :: ReleaseGroupCreateOpts -> Severity
  getSeverity ReleaseGroupCreateOpts{releaseGroupCommon = ReleaseGroupCommonOpts{debug}} = if debug then SevDebug else SevInfo

instance GetCommonOpts ReleaseGroupCommand

data ReleaseGroupProject = ReleaseGroupProject
  { projectName :: Text
  , projectRevision :: Text
  , projectBranch :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ReleaseGroupProject where
  toEncoding = genericToEncoding defaultOptions

mergeOpts ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  ReleaseGroupCommand ->
  m ReleaseGroupConfig
mergeOpts _ _ (ReleaseGroupCreate cliOpts) = do
  let logSeverity = getSeverity cliOpts
      title = titleOpts cliOpts
      release = releaseOpts cliOpts
      projects = projectsOpts cliOpts
      licenseCompliancePolicy = licenseCompliancePolicyOpts cliOpts
      vulnerabilityManagementPolicy = vulnerabilityManagementPolicyOpts cliOpts
      teams = teamsOpts cliOpts
  logDebug $ "THis is the create opts  -------- " <> pretty (pShow (cliOpts))
  pure . Create $ CreateConfig logSeverity title release projects licenseCompliancePolicy vulnerabilityManagementPolicy teams

releaseGroupCommonOpts :: Parser ReleaseGroupCommonOpts
releaseGroupCommonOpts =
  ReleaseGroupCommonOpts
    <$> switch (applyFossaStyle <> long "debug" <> stringToHelpDoc "Enable debug logging, and write detailed debug information to `fossa.debug.json`")
    <*> optional (uriOption (applyFossaStyle <> long "endpoint" <> short 'e' <> metavar "URL" <> helpDoc endpointHelp))
    <*> optional (strOption (applyFossaStyle <> long fossaApiKeyCmdText <> helpDoc fossaApiKeyHelp))
    <*> optional (strOption (applyFossaStyle <> long "config" <> short 'c' <> helpDoc configHelp))

releaseGroupProjectOpts :: Parser ReleaseGroupProject
releaseGroupProjectOpts =
  ReleaseGroupProject
    <$> strOption (applyFossaStyle <> long "project-name" <> stringToHelpDoc "The name of the FOSSA project you want to add to the FOSSA release group")
    <*> strOption (applyFossaStyle <> long "project-revision" <> stringToHelpDoc "The revision of the FOSSA project")
    <*> strOption (applyFossaStyle <> long "project-branch" <> stringToHelpDoc "The branch of the FOSSA project")

cliParser :: Parser ReleaseGroupCommand
cliParser = createSubCmd
  where
    createSubCmd :: Parser ReleaseGroupCommand
    createSubCmd = subparser . command "create" $ info (ReleaseGroupCreate <$> createOpts) releaseGroupCreateInfo

    createOpts :: Parser ReleaseGroupCreateOpts
    createOpts =
      ReleaseGroupCreateOpts
        <$> releaseGroupCommonOpts
        <*> strOption (applyFossaStyle <> long "title" <> short 't' <> stringToHelpDoc "The title of the FOSSA release group")
        <*> strOption (applyFossaStyle <> long "release" <> short 'r' <> stringToHelpDoc "The release of the FOSSA release group")
        <*> many (releaseGroupProjectOpts)
        <*> optional (strOption (applyFossaStyle <> long "license-compliance-policy-id" <> short 'l' <> stringToHelpDoc "The name of the license compliance policy you want to assign to the FOSSA release group"))
        <*> optional (strOption (applyFossaStyle <> long "vulnerability-management-policy-id" <> short 'v' <> stringToHelpDoc "The name of the vulnerability management policy you want to assign to the FOSSA release group"))
        <*> optional (many (strOption (applyFossaStyle <> long "team" <> short 'T' <> stringToHelpDoc "The team you want to assign to the FOSSA release group")))
