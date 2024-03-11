module App.Fossa.Config.ReleaseGroup.Common (
  ReleaseGroupCommonOpts (..),
  ReleaseGroupProjectOpts (..),
  releaseGroupCommonOpts,
  releaseGroupProjectOpts,
  collectApiOpts,
  mergeReleaseGroupTitle,
  mergeReleaseGroupRelease,
  mergeReleaseGroupProjectRevision,
) where

import App.Fossa.Config.Common (endpointHelp, fossaApiKeyCmdText, fossaApiKeyHelp)
import App.Fossa.Config.ConfigFile (ConfigFile (configApiKey, configServer), ConfigReleaseGroupProject (..))
import App.Fossa.Config.EnvironmentVars (EnvVars (..))
import App.OptionExtensions (uriOption)
import App.Types (ReleaseGroupProjectRevision (..))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, fatalText, fromMaybeText)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Text (Text, null, strip)
import Fossa.API.Types (ApiKey (ApiKey), ApiOpts (ApiOpts), defaultApiPollDelay)
import GHC.Generics (Generic)
import Options.Applicative (Parser, helpDoc, long, metavar, optional, short, strOption, switch, (<|>))
import Style (applyFossaStyle, stringToHelpDoc)
import Text.URI (URI, mkURI)

data ReleaseGroupCommonOpts = ReleaseGroupCommonOpts
  { debug :: Bool
  , baseUrl :: Maybe URI
  , apiKey :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data ReleaseGroupProjectOpts = ReleaseGroupProjectOpts
  { projectIdOpts :: Text
  , projectRevisionOpts :: Text
  , projectBranchOpts :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ReleaseGroupProjectOpts where
  toEncoding = genericToEncoding defaultOptions

releaseGroupCommonOpts :: Parser ReleaseGroupCommonOpts
releaseGroupCommonOpts =
  ReleaseGroupCommonOpts
    <$> switch (applyFossaStyle <> long "debug" <> stringToHelpDoc "Enable debug logging, and write detailed debug information to `fossa.debug.json`")
    <*> optional (uriOption (applyFossaStyle <> long "endpoint" <> short 'e' <> metavar "URL" <> helpDoc endpointHelp))
    <*> optional (strOption (applyFossaStyle <> long fossaApiKeyCmdText <> helpDoc fossaApiKeyHelp))

releaseGroupProjectOpts :: Parser ReleaseGroupProjectOpts
releaseGroupProjectOpts =
  ReleaseGroupProjectOpts
    <$> strOption (applyFossaStyle <> long "project-id" <> stringToHelpDoc "The id of the FOSSA project you want to add to the FOSSA release group")
    <*> strOption (applyFossaStyle <> long "project-revision" <> stringToHelpDoc "The revision of the FOSSA project")
    <*> strOption (applyFossaStyle <> long "project-branch" <> stringToHelpDoc "The branch of the FOSSA project")

validateApiKey :: (Has Diagnostics sig m) => Maybe ConfigFile -> EnvVars -> ReleaseGroupCommonOpts -> m ApiKey
validateApiKey maybeConfigFile EnvVars{envApiKey} ReleaseGroupCommonOpts{apiKey} = do
  textkey <-
    fromMaybeText "A FOSSA API key is required to run this command" $
      -- API key significance is strictly defined:
      -- 1. Cmd-line option (rarely used, not encouraged)
      -- 2. Config file (maybe used)
      -- 3. Environment Variable (most common)

      apiKey
        <|> (maybeConfigFile >>= configApiKey)
        <|> envApiKey
  if Data.Text.null . strip $ textkey
    then fatalText "A FOSSA API key was specified, but it is an empty string"
    else pure $ ApiKey textkey

collectApiOpts :: (Has Diagnostics sig m) => Maybe ConfigFile -> EnvVars -> ReleaseGroupCommonOpts -> m ApiOpts
collectApiOpts maybeconfig envvars commons = do
  apiKey <- validateApiKey maybeconfig envvars commons
  let configUri = maybeconfig >>= configServer >>= mkURI
      baseuri = baseUrl commons <|> configUri
  pure $ ApiOpts baseuri apiKey defaultApiPollDelay

mergeReleaseGroupTitle :: (Has Diagnostics sig m) => Maybe Text -> Maybe Text -> m Text
mergeReleaseGroupTitle maybeTitleOpts maybeTileConfig = case (maybeTitleOpts, maybeTileConfig) of
  (Just titleOpts, _) -> pure titleOpts
  (Nothing, Just titleConfig) -> pure titleConfig
  _ -> fatalText "You must specify a title to create a release group"

mergeReleaseGroupRelease :: (Has Diagnostics sig m) => Maybe Text -> Maybe Text -> m Text
mergeReleaseGroupRelease maybeReleaseOpts maybeReleaseConfig = case (maybeReleaseOpts, maybeReleaseConfig) of
  (Just releaseOpts, _) -> pure releaseOpts
  (Nothing, Just releaseConfig) -> pure releaseConfig
  _ -> fatalText "You must specify a title to create a release group"

mergeReleaseGroupProjectRevision :: (Has Diagnostics sig m) => Maybe [ReleaseGroupProjectOpts] -> Maybe [ConfigReleaseGroupProject] -> m [ReleaseGroupProjectRevision]
mergeReleaseGroupProjectRevision maybeProjectsFromOpts maybeProjectsFromConfig = case (maybeProjectsFromOpts, maybeProjectsFromConfig) of
  (Just projectsOpts, _) -> pure $ map convertProjectOpts projectsOpts
  (Nothing, Just projectsConfig) -> pure $ map convertProjectConfig projectsConfig
  (Nothing, Nothing) -> fatalText "You must specify at least one project to add to your release group"

convertProjectOpts :: ReleaseGroupProjectOpts -> ReleaseGroupProjectRevision
convertProjectOpts (ReleaseGroupProjectOpts projectId revision branch) = ReleaseGroupProjectRevision projectId (normalizeProjectRevision projectId revision) branch

convertProjectConfig :: ConfigReleaseGroupProject -> ReleaseGroupProjectRevision
convertProjectConfig (ConfigReleaseGroupProject projectId revision branch) = ReleaseGroupProjectRevision projectId (normalizeProjectRevision projectId revision) branch

normalizeProjectRevision :: Text -> Text -> Text
normalizeProjectRevision projectId revisionId = projectId <> "$" <> revisionId
