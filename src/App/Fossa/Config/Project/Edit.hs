{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Project.Edit (
  EditConfig (..),
  EditOpts (..),
  ProjectIdentifier (..),
  subcommand,
  cliParser,
  mergeOpts,
) where

import App.Fossa.Config.Common (configHelp, endpointHelp, fossaApiKeyCmdText, fossaApiKeyHelp, parsePolicyOptions, titleHelp, validateApiKeyGeneric)
import App.Fossa.Config.ConfigFile (ConfigFile (..), ConfigProject (..))
import App.Fossa.Config.EnvironmentVars (EnvVars (envApiKey))
import App.OptionExtensions (uriOption)
import App.Types (Policy (..))
import Control.Effect.Diagnostics (Diagnostics, Has, fatalText)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Data.Text (Text)
import Data.Text qualified
import Effect.Logger (vsep)
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
  metavar,
  optional,
  short,
  some,
  strOption,
  switch,
  (<|>),
 )
import Options.Applicative.Builder (progDescDoc)
import Options.Applicative.Help (AnsiStyle)
import Prettyprinter (Doc)
import Style (applyFossaStyle, boldItalicized, formatDoc, formatStringToDoc, stringToHelpDoc)
import Text.URI (URI, mkURI)

projectEditInfo :: InfoMod a
projectEditInfo = progDescDoc $ formatStringToDoc "Edit a FOSSA project"

subcommand :: (EditOpts -> a) -> Mod CommandFields a
subcommand f = command "edit" $ info (f <$> cliParser) projectEditInfo

data EditConfig = EditConfig
  { apiOpts :: ApiOpts
  , projectIdentifier :: ProjectIdentifier
  , projectTitle :: Maybe Text
  , projectUrl :: Maybe Text
  , projectJiraKey :: Maybe Text
  , projectPolicy :: Maybe Policy
  , projectLink :: Maybe Text
  , projectLabels :: Maybe [Text]
  , -- Teams to add project
    teams :: Maybe [Text]
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON EditConfig where
  toEncoding = genericToEncoding defaultOptions

data ProjectIdentifier
  = ProjectId Text
  | ProjectLocator Text
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ProjectIdentifier where
  toEncoding = genericToEncoding defaultOptions

data EditOpts = EditOpts
  { debug :: Bool
  , baseUrlOpts :: Maybe URI
  , apiKeyOpts :: Maybe Text
  , configOpts :: Maybe FilePath
  , projectIdentiferOpts :: Maybe ProjectIdentifier
  , projectTitleOpts :: Maybe Text
  , projectUrlOpts :: Maybe Text
  , projectJiraKeyOpts :: Maybe Text
  , projectPolicyOpts :: Maybe Policy
  , projectLinkOpts :: Maybe Text
  , projectLabelsOpts :: Maybe [Text]
  , teamsOpts :: Maybe [Text]
  }
  deriving (Eq, Ord, Show, Generic)

cliParser :: Parser EditOpts
cliParser =
  EditOpts
    <$> switch (applyFossaStyle <> long "debug" <> stringToHelpDoc "Enable debug logging")
    <*> optional (uriOption (applyFossaStyle <> long "endpoint" <> short 'e' <> metavar "URL" <> helpDoc endpointHelp))
    <*> optional (strOption (applyFossaStyle <> long fossaApiKeyCmdText <> helpDoc fossaApiKeyHelp))
    <*> optional (strOption (applyFossaStyle <> long "config" <> short 'c' <> helpDoc configHelp))
    <*> projectIdentiferOptions
    <*> optional (strOption (applyFossaStyle <> long "title" <> short 't' <> helpDoc titleHelp))
    <*> optional (strOption (applyFossaStyle <> long "project-url" <> short 'P' <> stringToHelpDoc "The url of the project's repository."))
    <*> optional (strOption (applyFossaStyle <> long "jira-project-key" <> short 'j' <> stringToHelpDoc "The JIRA project key to associate with the project."))
    <*> parsePolicyOptions
    <*> optional (strOption (applyFossaStyle <> long "link" <> short 'L' <> stringToHelpDoc "A link to attach to the project."))
    <*> optional (some (strOption (applyFossaStyle <> long "project-label" <> stringToHelpDoc "Assign up to 5 labels to the project.")))
    <*> optional (some (strOption (applyFossaStyle <> long "team" <> short 'T' <> stringToHelpDoc "The teams to associate with the project.")))

projectIdHelp :: Maybe (Doc AnsiStyle)
projectIdHelp =
  Just . formatDoc $
    vsep
      [ "The project ID defines an ID that is used to reference a project within your FOSSA organization."
      , ""
      , "If the project ID was not explicitly set during the time of project creation, it defaults to:"
      , boldItalicized "Git: " <> "The .git/config file or project's remote `origin` URL"
      , boldItalicized "SVN: " <> "The `Repository Root` obtained using 'svn info'"
      , boldItalicized "No VCS: " <> "The name of the project's directory"
      ]

projectIdOptions :: Parser ProjectIdentifier
projectIdOptions = ProjectId <$> (strOption (applyFossaStyle <> long "project-id" <> helpDoc projectIdHelp))

projectLocatorOptions :: Parser ProjectIdentifier
projectLocatorOptions = ProjectLocator <$> (strOption (applyFossaStyle <> long "project-locator" <> stringToHelpDoc "The project locator defines a unique ID that the FOSSA API will use to reference this project within FOSSA."))

projectIdentiferOptions :: Parser (Maybe ProjectIdentifier)
projectIdentiferOptions = optional (projectIdOptions <|> projectLocatorOptions)

mergeOpts ::
  ( Has Diagnostics sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  EditOpts ->
  m EditConfig
mergeOpts maybeConfig envVars cliOpts@EditOpts{..} = do
  apiOpts <- collectApiOpts maybeConfig envVars cliOpts
  maybePolicy <- maybe (pure projectPolicyOpts) (mergePolicy projectPolicyOpts) maybeConfig
  let maybeProjectIdentifer = maybe projectIdentiferOpts (mergeProjectIdentifier projectIdentiferOpts) maybeConfig

  case maybeProjectIdentifer of
    Just projId@(ProjectId projectId')
      | not (Data.Text.null projectId') ->
          pure $ mkConfig apiOpts projId maybePolicy
    Just projId@(ProjectLocator locatorVal)
      | not (Data.Text.null locatorVal) ->
          pure $ mkConfig apiOpts projId maybePolicy
    _ -> fatalText "Either projectId or project locator needs to be specified to edit a FOSSA project"
  where
    mkConfig :: ApiOpts -> ProjectIdentifier -> Maybe Policy -> EditConfig
    mkConfig apiOpts projectIdentifer maybePolicy = do
      let maybeProjectConfig = maybeConfig >>= configProject
          configProjectLabels = maybe [] configLabel maybeProjectConfig
      EditConfig
        { apiOpts = apiOpts
        , projectIdentifier = projectIdentifer
        , projectTitle = projectTitleOpts <|> extractProjectConfigVal maybeConfig configName
        , projectUrl = projectUrlOpts <|> extractProjectConfigVal maybeConfig configUrl
        , projectJiraKey = projectJiraKeyOpts <|> extractProjectConfigVal maybeConfig configJiraKey
        , projectPolicy = maybePolicy
        , projectLink = projectLinkOpts <|> extractProjectConfigVal maybeConfig configLink
        , projectLabels = projectLabelsOpts <|> labelConfigToMaybe configProjectLabels
        , -- Teams to add project to
          teams = teamsOpts <|> extractProjectConfigVal maybeConfig configTeams
        }

    labelConfigToMaybe :: [Text] -> Maybe [Text]
    labelConfigToMaybe [] = Nothing
    labelConfigToMaybe xs = Just xs

collectApiOpts :: (Has Diagnostics sig m) => Maybe ConfigFile -> EnvVars -> EditOpts -> m ApiOpts
collectApiOpts maybeConfig envVars EditOpts{..} = do
  apikey <- validateApiKeyGeneric maybeConfig (envApiKey envVars) apiKeyOpts
  let configUri = maybeConfig >>= configServer >>= mkURI
      baseuri = baseUrlOpts <|> configUri
  pure $ ApiOpts baseuri apikey defaultApiPollDelay

extractProjectConfigVal :: Maybe ConfigFile -> (ConfigProject -> Maybe a) -> Maybe a
extractProjectConfigVal maybeConfig getValue = do
  let projectCfg = maybeConfig >>= configProject
  projectCfg >>= getValue

mergeProjectIdentifier :: Maybe ProjectIdentifier -> ConfigFile -> Maybe ProjectIdentifier
mergeProjectIdentifier maybeProjectIdentifer config = maybeProjectIdentifer <|> ProjectId <$> (configProject config >>= configProjID) <|> ProjectLocator <$> (configProject config >>= configProjLocator)

mergePolicy :: Has Diagnostics sig m => Maybe Policy -> ConfigFile -> m (Maybe Policy)
mergePolicy maybePolicy config =
  case (maybePolicy, cfgPolicy) of
    (Just (PolicyId _), Just (PolicyName _)) -> err
    (Just (PolicyName _), Just (PolicyId _)) -> err
    _ -> pure $ maybePolicy <|> cfgPolicy
  where
    err = fatalText "Only one of policy or policyId can be set. Check your cli options and .fossa.yml to ensure you aren't specifying both."
    cfgPolicy = configProject config >>= configPolicy
