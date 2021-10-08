{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module App.Fossa.Configuration (
  mergeFileCmdMetadata,
  readConfigFileIO,
  readConfigFile,
  ConfigFile (..),
  ConfigProject (..),
  ConfigRevision (..),
  ConfigTargets (..),
  ConfigPaths (..),
) where

import App.Docs (fossaYmlDocUrl)
import App.Types
import Control.Carrier.Diagnostics qualified as Diag
import Control.Effect.Lift (Lift)
import Data.Aeson (FromJSON (parseJSON), withObject, (.!=), (.:), (.:?))
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty (pretty), vsep)
import Effect.Logger (Severity (SevWarn), logWarn, withDefaultLogger)
import Effect.ReadFS
import Path
import Path.IO (getCurrentDir)
import System.Exit (die)
import Text.Megaparsec
import Types

data ConfigFile = ConfigFile
  { configVersion :: Int
  , configServer :: Maybe Text
  , configApiKey :: Maybe Text
  , configProject :: Maybe ConfigProject
  , configRevision :: Maybe ConfigRevision
  , configTargets :: Maybe ConfigTargets
  , configPaths :: Maybe ConfigPaths
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
  , configReleaseGroup :: Maybe ReleaseGroupMetadata
  }
  deriving (Eq, Ord, Show)

data ConfigRevision = ConfigRevision
  { configCommit :: Maybe Text
  , configBranch :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data ConfigTargets = ConfigTargets
  { targetsOnly :: [TargetFilter]
  , targetsExclude :: [TargetFilter]
  }
  deriving (Eq, Ord, Show)

data ConfigPaths = ConfigPaths
  { pathsOnly :: [Path Rel Dir]
  , pathsExclude :: [Path Rel Dir]
  }
  deriving (Eq, Ord, Show)

instance FromJSON ConfigFile where
  parseJSON = withObject "ConfigFile" $ \obj ->
    ConfigFile <$> obj .: "version"
      <*> obj .:? "server"
      <*> obj .:? "apiKey"
      <*> obj .:? "project"
      <*> obj .:? "revision"
      <*> obj .:? "targets"
      <*> obj .:? "paths"

instance FromJSON ConfigProject where
  parseJSON = withObject "ConfigProject" $ \obj ->
    ConfigProject <$> obj .:? "id"
      <*> obj .:? "name"
      <*> obj .:? "link"
      <*> obj .:? "team"
      <*> obj .:? "jiraProjectKey"
      <*> obj .:? "url"
      <*> obj .:? "policy"
      <*> obj .:? "releaseGroup"

instance FromJSON ConfigRevision where
  parseJSON = withObject "ConfigRevision" $ \obj ->
    ConfigRevision <$> obj .:? "commit"
      <*> obj .:? "branch"

instance FromJSON ConfigTargets where
  parseJSON = withObject "ConfigTargets" $ \obj ->
    ConfigTargets <$> (obj .:? "only" .!= [])
      <*> (obj .:? "exclude" .!= [])

instance FromJSON ConfigPaths where
  parseJSON = withObject "ConfigPaths" $ \obj ->
    ConfigPaths <$> (obj .:? "only" .!= [])
      <*> (obj .:? "exclude" .!= [])

defaultFile :: Path Rel File
defaultFile = $(mkRelFile ".fossa.yml")

readConfigFile :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diag.Diagnostics sig m) => Path Abs File -> m (Maybe ConfigFile)
readConfigFile file = do
  exists <- doesFileExist file
  if not exists
    then pure Nothing
    else do
      readConfig <- readContentsYaml @ConfigFile file
      if configVersion readConfig < 3
        then withDefaultLogger SevWarn (logWarn $ warnMsgForOlderConfig (configVersion readConfig)) $> Nothing
        else pure $ Just readConfig
  where
    warnMsgForOlderConfig :: Int -> Doc ann
    warnMsgForOlderConfig foundVersion =
      vsep
        [ ""
        , "Incompatible [.fossa.yml] found! Expecting `version: 3`; found `version: " <> pretty foundVersion <> "`"
        , "Documentation for the new config file format can be found here:"
        , "    " <> pretty fossaYmlDocUrl
        , ""
        ]

readConfigFileIO :: IO (Maybe ConfigFile)
readConfigFileIO = do
  -- FIXME: we probably want to read from the target directory of analysis, not
  -- the current directory
  dir <- getCurrentDir
  config <- Diag.runDiagnostics $ runReadFSIO $ readConfigFile (dir </> defaultFile)
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
    , projectReleaseGroup = projectReleaseGroup meta <|> (configProject file >>= configReleaseGroup)
    }
