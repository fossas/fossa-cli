module App.Fossa.Config.ReleaseGroup.CreateRelease (
  CreateReleaseConfig (..),
  CreateReleaseOpts (..),
  cliParser,
  mergeOpts,
  subcommand,
) where

import App.Fossa.Config.Common (configFileOpt)
import App.Fossa.Config.ConfigFile (ConfigFile, ConfigReleaseGroup (..))
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Config.ReleaseGroup.Common qualified as Common
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

releaseGroupCreateReleaseInfo :: InfoMod a
releaseGroupCreateReleaseInfo = progDescDoc $ formatStringToDoc "Create a FOSSA release group release"

subcommand :: (CreateReleaseOpts -> a) -> Mod CommandFields a
subcommand f = command "create-release" $ info (f <$> cliParser) releaseGroupCreateReleaseInfo

data CreateReleaseConfig = CreateReleaseConfig
  { apiOpts :: ApiOpts
  , releaseGroupTitle :: Text
  , releaseGroupReleaseRevision :: ReleaseGroupReleaseRevision
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON CreateReleaseConfig where
  toEncoding = genericToEncoding defaultOptions

data CreateReleaseOpts = CreateReleaseOpts
  { releaseGroupCommon :: Common.ReleaseGroupCommonOpts
  , configOpts :: Maybe FilePath
  , releaseGroupTitleOpts :: Maybe Text
  , releaseGroupReleaseTitleOpts :: Maybe Text
  , projectsOpts :: Maybe [Common.ReleaseGroupProjectOpts]
  }
  deriving (Eq, Ord, Show, Generic)

cliParser :: Parser CreateReleaseOpts
cliParser =
  CreateReleaseOpts
    <$> Common.releaseGroupCommonOpts
    <*> configFileOpt
    <*> optional Common.releaseGroupTitleOpts
    <*> optional Common.releaseGroupReleaseTitleOpts
    <*> optional (some (Common.releaseGroupProjectOpts))

mergeOpts ::
  (Has Diagnostics sig m) =>
  Maybe FilePath ->
  Maybe ConfigFile ->
  EnvVars ->
  CreateReleaseOpts ->
  m CreateReleaseConfig
mergeOpts _ maybeConfig envVars cliOpts@CreateReleaseOpts{releaseGroupCommon, releaseGroupTitleOpts} = do
  apiOpts <- Common.collectApiOpts maybeConfig envVars releaseGroupCommon
  releaseGroupTitle <- Common.mergeReleaseGroupTitle releaseGroupTitleOpts $ Common.extractReleaseGroupConfigValue maybeConfig configReleaseGroupTitle
  releaseGroupRevision <- collectReleaseGroupRevision maybeConfig cliOpts
  pure $ CreateReleaseConfig apiOpts releaseGroupTitle releaseGroupRevision

collectReleaseGroupRevision :: (Has Diagnostics sig m) => Maybe ConfigFile -> CreateReleaseOpts -> m ReleaseGroupReleaseRevision
collectReleaseGroupRevision maybeConfig CreateReleaseOpts{releaseGroupReleaseTitleOpts, projectsOpts} = do
  releaseTitle <- Common.mergeReleaseGroupRelease releaseGroupReleaseTitleOpts $ Common.extractReleaseGroupConfigValue maybeConfig configReleaseGroupRelease
  projects <- Common.mergeReleaseGroupProjectRevision projectsOpts $ Common.extractReleaseGroupConfigValue maybeConfig configReleaseGroupProjects

  pure $
    ReleaseGroupReleaseRevision
      { releaseTitle = releaseTitle
      , releaseProjects = projects
      }
