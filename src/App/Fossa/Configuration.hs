{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module App.Fossa.Configuration (
  mergeFileCmdMetadata,
  readConfigFileIO,
  readConfigFile,
  ConfigFile (..),
  ConfigProject (..),
  ConfigRevision (..),
) where

import App.Types
import Control.Applicative (Alternative ((<|>)))
import Control.Carrier.Diagnostics qualified as Diag
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Text (Text)
import Effect.ReadFS
import Path
import System.Exit (die)

data ConfigFile = ConfigFile
  { configVersion :: Int
  , configServer :: Maybe Text
  , configApiKey :: Maybe Text
  , configProject :: Maybe ConfigProject
  , configRevision :: Maybe ConfigRevision
  }
  deriving (Eq, Ord, Show)

data ConfigProject = ConfigProject
  { configProjID :: Maybe Text
  , configName :: Maybe Text
  , configLink :: Maybe Text
  , configTeam :: Maybe Text
  , configJiraKey :: Maybe Text
  , configUrl :: Maybe Text
  , configPolicy :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data ConfigRevision = ConfigRevision
  { configCommit :: Maybe Text
  , configBranch :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON ConfigFile where
  parseJSON = withObject "ConfigFile" $ \obj ->
    ConfigFile <$> obj .: "version"
      <*> obj .:? "server"
      <*> obj .:? "apiKey"
      <*> obj .:? "project"
      <*> obj .:? "revision"

instance FromJSON ConfigProject where
  parseJSON = withObject "ConfigProject" $ \obj ->
    ConfigProject <$> obj .:? "id"
      <*> obj .:? "name"
      <*> obj .:? "link"
      <*> obj .:? "team"
      <*> obj .:? "jiraProjectKey"
      <*> obj .:? "url"
      <*> obj .:? "policy"

instance FromJSON ConfigRevision where
  parseJSON = withObject "ConfigRevision" $ \obj ->
    ConfigRevision <$> obj .:? "commit"
      <*> obj .:? "branch"

defaultFile :: Path Rel File
defaultFile = $(mkRelFile ".fossa.yml")

readConfigFile :: (Has ReadFS sig m, Has Diag.Diagnostics sig m) => Path Rel File -> m (Maybe ConfigFile)
readConfigFile file = do
  exists <- doesFileExist file
  if not exists
    then pure Nothing
    else do
      readConfig <- readContentsYaml @ConfigFile file
      if configVersion readConfig < 3
        then pure Nothing
        else pure $ Just readConfig

readConfigFileIO :: IO (Maybe ConfigFile)
readConfigFileIO = do
  config <- Diag.runDiagnostics $ runReadFSIO $ readConfigFile defaultFile
  case config of
    Left err -> die $ show $ Diag.renderFailureBundle err
    Right a -> pure a

mergeFileCmdMetadata :: ProjectMetadata -> ConfigFile -> ProjectMetadata
mergeFileCmdMetadata meta file =
  ProjectMetadata
    { projectTitle = projectTitle meta <|> (configProject file >>= configName)
    , projectUrl = projectUrl meta <|> (configProject file >>= configUrl)
    , projectJiraKey = projectJiraKey meta <|> (configProject file >>= configJiraKey)
    , projectLink = projectLink meta <|> (configProject file >>= configLink)
    , projectTeam = projectTeam meta <|> (configProject file >>= configTeam)
    , projectPolicy = projectPolicy meta <|> (configProject file >>= configPolicy)
    }
