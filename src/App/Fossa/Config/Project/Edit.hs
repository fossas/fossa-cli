{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Project.Edit (
  EditConfig (..),
  EditOpts (..),
  subcommand,
  cliParser,
  mergeOpts,
) where

import App.Fossa.Config.Common (configHelp, endpointHelp, fossaApiKeyCmdText, fossaApiKeyHelp, parsePolicyOptions, titleHelp, validateApiKeyGeneric)
import App.Fossa.Config.ConfigFile (ConfigFile (..), ConfigProject (..))
import App.Fossa.Config.EnvironmentVars (EnvVars (envApiKey))
import App.OptionExtensions (uriOption)
import App.Types (Policy (..), ProjectMetadataRevision (..))
import Control.Effect.Diagnostics (Diagnostics, Has, fatalText)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Data.Text (Text)
import Data.Text qualified
import Fossa.API.Types (ApiOpts (..), defaultApiPollDelay)
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
  metavar,
  optional,
  short,
  strOption,
  switch,
  (<|>),
 )
import Options.Applicative.Builder (progDescDoc)
import Style (applyFossaStyle, formatStringToDoc, stringToHelpDoc)
import Text.URI (URI, mkURI)

projectEditInfo :: InfoMod a
projectEditInfo = progDescDoc $ formatStringToDoc "Edit a FOSSA project"

subcommand :: (EditOpts -> a) -> Mod CommandFields a
subcommand f = command "edit" $ info (f <$> cliParser) projectEditInfo

data EditConfig = EditConfig
  { apiOpts :: ApiOpts
  , projectLocator :: Text
  , projectMetadataRevision :: ProjectMetadataRevision
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON EditConfig where
  toEncoding = genericToEncoding defaultOptions

data EditOpts = EditOpts
  { debug :: Bool
  , baseUrlOpts :: Maybe URI
  , apiKeyOpts :: Maybe Text
  , configOpts :: Maybe FilePath
  , projectLocatorOpts :: Maybe Text
  , projectMetadataRevisionOpts :: ProjectMetadataRevision
  }
  deriving (Eq, Ord, Show, Generic)

cliParser :: Parser EditOpts
cliParser =
  EditOpts
    <$> switch (applyFossaStyle <> long "debug" <> stringToHelpDoc "Enable debug logging")
    <*> optional (uriOption (applyFossaStyle <> long "endpoint" <> short 'e' <> metavar "URL" <> helpDoc endpointHelp))
    <*> optional (strOption (applyFossaStyle <> long fossaApiKeyCmdText <> helpDoc fossaApiKeyHelp))
    <*> optional (strOption (applyFossaStyle <> long "config" <> short 'c' <> helpDoc configHelp))
    <*> optional (strOption (applyFossaStyle <> long "project-locator" <> stringToHelpDoc "The project locator defines a unique ID that the FOSSA API will use to reference this project within FOSSA."))
    <*> projectOpts

projectOpts :: Parser ProjectMetadataRevision
projectOpts =
  ProjectMetadataRevision
    <$> optional (strOption (applyFossaStyle <> long "title" <> short 't' <> helpDoc titleHelp))
    <*> optional (strOption (applyFossaStyle <> long "project-url" <> short 'P' <> stringToHelpDoc "The url of the project's repository."))
    <*> optional (strOption (applyFossaStyle <> long "jira-project-key" <> short 'j' <> stringToHelpDoc "The JIRA project key to associate with the project."))
    <*> optional (strOption (applyFossaStyle <> long "link" <> short 'L' <> stringToHelpDoc "A link to attach to the project."))
    <*> optional (strOption (applyFossaStyle <> long "team" <> short 'T' <> stringToHelpDoc "The team to associate with the project."))
    <*> parsePolicyOptions
    <*> many (strOption (applyFossaStyle <> long "project-label" <> stringToHelpDoc "Assign up to 5 labels to the project."))

mergeOpts ::
  ( Has Diagnostics sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  EditOpts ->
  m EditConfig
mergeOpts maybeConfig envVars cliOpts@EditOpts{..} = do
  let maybeProjectLocator = maybe (projectLocatorOpts) (mergeProjectLocator projectLocatorOpts) maybeConfig
  apiOpts <- collectApiOpts maybeConfig envVars cliOpts
  projectMetadataRevision <- maybe (pure projectMetadataRevisionOpts) (mergeProjectMetadata projectMetadataRevisionOpts) maybeConfig
  case maybeProjectLocator of
    Just projectLocator' | not (Data.Text.null projectLocator') -> pure $ EditConfig apiOpts projectLocator' projectMetadataRevision
    _ -> fatalText "A project locator needs to be specified to edit a FOSSA project"

collectApiOpts :: (Has Diagnostics sig m) => Maybe ConfigFile -> EnvVars -> EditOpts -> m ApiOpts
collectApiOpts maybeConfig envVars EditOpts{..} = do
  apikey <- validateApiKeyGeneric maybeConfig (envApiKey envVars) apiKeyOpts
  let configUri = maybeConfig >>= configServer >>= mkURI
      baseuri = baseUrlOpts <|> configUri
  pure $ ApiOpts baseuri apikey defaultApiPollDelay

mergeProjectLocator :: Maybe Text -> ConfigFile -> Maybe Text
mergeProjectLocator maybeProjectLocator config = maybeProjectLocator <> (configProject config >>= configProjLocator)

mergeProjectMetadata :: Has Diagnostics sig m => ProjectMetadataRevision -> ConfigFile -> m ProjectMetadataRevision
mergeProjectMetadata ProjectMetadataRevision{..} config =
  case (projectPolicyRevision, cfgPolicy) of
    (Just (PolicyId _), Just (PolicyName _)) -> err
    (Just (PolicyName _), Just (PolicyId _)) -> err
    _ -> pure . mkMeta $ projectPolicyRevision <|> cfgPolicy
  where
    err = fatalText "Only one of policy or policyId can be set. Check your cli options and .fossa.yml to ensure you aren't specifying both."
    cfgPolicy = configProject config >>= configPolicy

    mkMeta :: Maybe Policy -> ProjectMetadataRevision
    mkMeta policy =
      ProjectMetadataRevision
        { projectTitleRevision = projectTitleRevision <|> (configProject config >>= configName)
        , projectUrlRevision = projectUrlRevision <|> (configProject config >>= configUrl)
        , projectJiraKeyRevision = projectJiraKeyRevision <|> (configProject config >>= configJiraKey)
        , projectLinkRevision = projectLinkRevision <|> (configProject config >>= configLink)
        , projectTeamRevision = projectTeamRevision <|> (configProject config >>= configTeam)
        , projectPolicyRevision = policy
        , projectLabelRevision = projectLabelRevision <|> (maybe [] configLabel (configProject config))
        }
