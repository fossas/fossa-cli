module App.Fossa.Config.ReleaseGroup.Common (
  ReleaseGroupCommonOpts (..),
  ReleaseGroupProjectOpts (..),
  releaseGroupCommonOpts,
  releaseGroupProjectOpts,
  collectApiOpts,
  mergeReleaseGroupTitle,
  mergeReleaseGroupRelease,
  mergeReleaseGroupProjectRevision,
  extractReleaseGroupConfigValue,
  releaseGroupTitleOpts,
  releaseGroupReleaseTitleOpts,
) where

import App.Fossa.Config.Common (apiKeyOpt, endpointOpt)
import App.Fossa.Config.ConfigFile (ConfigFile (configApiKey, configReleaseGroup, configServer), ConfigReleaseGroup, ConfigReleaseGroupProject (..))
import App.Fossa.Config.EnvironmentVars (EnvVars (..))
import App.Types (ReleaseGroupProjectRevision (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, fatalText, fromMaybeText)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Text (Text, null, strip)
import Fossa.API.Types (ApiKey (ApiKey), ApiOpts (ApiOpts), defaultApiPollDelay)
import GHC.Generics (Generic)
import Options.Applicative (Parser, long, short, strOption, switch, (<|>))
import Style (applyFossaStyle, stringToHelpDoc)
import Text.URI (URI, mkURI)

data ReleaseGroupCommonOpts = ReleaseGroupCommonOpts
  { debug :: Bool
  , baseUrl :: Maybe URI
  , apiKey :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data ReleaseGroupProjectOpts = ReleaseGroupProjectOpts
  { projectLocatorOpts :: Text
  , projectRevisionOpts :: Text
  , projectBranchOpts :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ReleaseGroupProjectOpts where
  toEncoding = genericToEncoding defaultOptions

releaseGroupCommonOpts :: Parser ReleaseGroupCommonOpts
releaseGroupCommonOpts =
  ReleaseGroupCommonOpts
    <$> switch (applyFossaStyle <> long "debug" <> stringToHelpDoc "Enable debug logging")
    <*> endpointOpt
    <*> apiKeyOpt

releaseGroupProjectOpts :: Parser ReleaseGroupProjectOpts
releaseGroupProjectOpts =
  ReleaseGroupProjectOpts
    <$> strOption (applyFossaStyle <> long "project-locator" <> stringToHelpDoc "The project Locator defines a unique ID that the FOSSA API will use to reference this project within FOSSA")
    <*> strOption (applyFossaStyle <> long "project-revision" <> stringToHelpDoc "The revision of the FOSSA project")
    <*> strOption (applyFossaStyle <> long "project-branch" <> stringToHelpDoc "The branch of the FOSSA project")

releaseGroupTitleOpts :: Parser Text
releaseGroupTitleOpts = strOption (applyFossaStyle <> long "title" <> short 't' <> stringToHelpDoc "The title of the FOSSA release group")

releaseGroupReleaseTitleOpts :: Parser Text
releaseGroupReleaseTitleOpts = strOption (applyFossaStyle <> long "release" <> short 'r' <> stringToHelpDoc "The release of the FOSSA release group")

validateApiKey :: (Has Diagnostics sig m) => Maybe ConfigFile -> EnvVars -> ReleaseGroupCommonOpts -> m ApiKey
validateApiKey maybeConfigFile EnvVars{envApiKey} ReleaseGroupCommonOpts{apiKey} = do
  textkey <-
    fromMaybeText "A FOSSA API key is required to run this command" $
      -- API key precedence is strictly defined:
      -- 1. Cmd-line option (rarely used, not encouraged)
      -- 2. Config file (maybe used)
      -- 3. Environment Variable (most common)

      apiKey
        <|> (maybeConfigFile >>= configApiKey)
        <|> envApiKey
  if Data.Text.null . strip $ textkey
    then fatalText "A FOSSA API key was specified, but it is an empty string"
    else pure $ ApiKey textkey

extractReleaseGroupConfigValue :: Maybe ConfigFile -> (ConfigReleaseGroup -> Maybe a) -> Maybe a
extractReleaseGroupConfigValue maybeConfig getValue = do
  let releaseGroupCfg = maybeConfig >>= configReleaseGroup
  releaseGroupCfg >>= getValue

collectApiOpts :: (Has Diagnostics sig m) => Maybe ConfigFile -> EnvVars -> ReleaseGroupCommonOpts -> m ApiOpts
collectApiOpts maybeconfig envvars commons = do
  apiKey <- validateApiKey maybeconfig envvars commons
  let configUri = maybeconfig >>= configServer >>= mkURI
      baseuri = baseUrl commons <|> configUri
  pure $ ApiOpts baseuri apiKey defaultApiPollDelay

mergeReleaseGroupTitle :: (Has Diagnostics sig m) => Maybe Text -> Maybe Text -> m Text
mergeReleaseGroupTitle maybeTitleOpts maybeTileConfig = case (maybeTitleOpts, maybeTileConfig) of
  (Just titleOpts, _) | not (Data.Text.null titleOpts) -> pure titleOpts
  (Nothing, Just titleConfig) | not (Data.Text.null titleConfig) -> pure titleConfig
  _ -> fatalText "You must specify a release group title"

mergeReleaseGroupRelease :: (Has Diagnostics sig m) => Maybe Text -> Maybe Text -> m Text
mergeReleaseGroupRelease maybeReleaseOpts maybeReleaseConfig = case (maybeReleaseOpts, maybeReleaseConfig) of
  (Just releaseOpts, _) | not (Data.Text.null releaseOpts) -> pure releaseOpts
  (Nothing, Just releaseConfig) | not (Data.Text.null releaseConfig) -> pure releaseConfig
  _ -> fatalText "You must specify a release title"

mergeReleaseGroupProjectRevision :: (Has Diagnostics sig m) => Maybe [ReleaseGroupProjectOpts] -> Maybe [ConfigReleaseGroupProject] -> m [ReleaseGroupProjectRevision]
mergeReleaseGroupProjectRevision maybeProjectsFromOpts maybeProjectsFromConfig = case (maybeProjectsFromOpts, maybeProjectsFromConfig) of
  (Just projectsOpts, _) -> pure $ map convertProjectOpts projectsOpts
  (Nothing, Just projectsConfig) -> pure $ map convertProjectConfig projectsConfig
  (Nothing, Nothing) -> fatalText "You must specify at least one project to add to your release group release"

convertProjectOpts :: ReleaseGroupProjectOpts -> ReleaseGroupProjectRevision
convertProjectOpts (ReleaseGroupProjectOpts projectLocator revision branch) = ReleaseGroupProjectRevision projectLocator (normalizeProjectRevision projectLocator revision) branch

convertProjectConfig :: ConfigReleaseGroupProject -> ReleaseGroupProjectRevision
convertProjectConfig (ConfigReleaseGroupProject projectLocator revision branch) = ReleaseGroupProjectRevision projectLocator (normalizeProjectRevision projectLocator revision) branch

normalizeProjectRevision :: Text -> Text -> Text
normalizeProjectRevision projectLocator revisionId = projectLocator <> "$" <> revisionId
