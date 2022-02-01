{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Config.ConfigFile (
  defaultConfigFileName,
  resolveConfigFile,
  ConfigFile (..),
  ConfigProject (..),
  ConfigRevision (..),
  ConfigTargets (..),
  ConfigPaths (..),
  ExperimentalConfigs (..),
  ExperimentalGradleConfigs (..),
  mergeFileCmdMetadata,
  empty,
) where

import App.Docs (fossaYmlDocUrl)
import App.Types (ProjectMetadata (..), ReleaseGroupMetadata)
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  fatal,
  fatalText,
  fromEitherShow,
 )
import Control.Effect.Lift (Lift)
import Data.Aeson (
  FromJSON (parseJSON),
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Functor (($>))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import Effect.Logger (
  AnsiStyle,
  Doc,
  Logger,
  Pretty (pretty),
  logWarn,
  vsep,
 )
import Effect.ReadFS (ReadFS, doesFileExist, readContentsYaml)
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  SomeBase (Abs, Rel),
  mkRelFile,
  parseSomeFile,
  (</>),
 )
import Types (TargetFilter)

defaultConfigFileName :: Path Rel File
defaultConfigFileName = $(mkRelFile ".fossa.yml")

-- data ConfigLocation = ConfigLocation
--   { -- | Did the user specify the path (True/required), or is this default (False/optional)?
--     isRequired :: Bool
--   , -- | What is the absolute file path?
--     configFilePath :: Path Abs File
--   }

data ConfigLocation
  = DefaultConfigLocation (Path Abs Dir)
  | SpecifiedConfigLocation (Path Abs File)

resolveConfigFile ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  -- | Where do we look for a config file if relative?
  Path Abs Dir ->
  -- | Is there a specified file to load?
  Maybe FilePath ->
  m (Maybe ConfigFile)
resolveConfigFile base path = do
  loc <- resolveLocation base path
  case loc of
    DefaultConfigLocation dir -> do
      let realpath = dir </> defaultConfigFileName
      exists <- doesFileExist realpath
      if not exists
        then -- File missing and not requested
          pure Nothing
        else do
          configFile <- readContentsYaml realpath
          let version = configVersion configFile
          if version >= 3
            then pure $ Just configFile
            else -- Invalid config found without --config flag: warn and ignore file.
              logWarn (warnMsgForOlderConfig @AnsiStyle version) $> Nothing
    SpecifiedConfigLocation realpath -> do
      exists <- doesFileExist realpath
      if not exists
        then -- file requested, but missing
          fatalText ("requested config file does not exist: " <> toText realpath)
        else do
          configFile <- readContentsYaml realpath
          let version = configVersion configFile
          if version >= 3
            then pure $ Just configFile
            else -- Invalid config with --config specified: fail with message.
              fatal $ warnMsgForOlderConfig @AnsiStyle version

warnMsgForOlderConfig :: Int -> Doc ann
warnMsgForOlderConfig foundVersion =
  vsep
    [ ""
    , "Incompatible [.fossa.yml] found! Expecting `version: 3`; found `version: " <> pretty foundVersion <> "`"
    , "Documentation for the new config file format can be found here:"
    , "    " <> pretty fossaYmlDocUrl
    , ""
    ]

resolveLocation :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs Dir -> Maybe FilePath -> m ConfigLocation
resolveLocation base Nothing = pure $ DefaultConfigLocation base
resolveLocation base (Just filepath) = do
  someFile <- context "Parsing `--config` option as a file" $ fromEitherShow $ parseSomeFile filepath
  pure $
    SpecifiedConfigLocation $ case someFile of
      Abs path -> path
      Rel path -> base </> path

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

empty :: ConfigFile
empty = ConfigFile 3 Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data ConfigFile = ConfigFile
  { configVersion :: Int
  , configServer :: Maybe Text
  , configApiKey :: Maybe Text
  , configProject :: Maybe ConfigProject
  , configRevision :: Maybe ConfigRevision
  , configTargets :: Maybe ConfigTargets
  , configPaths :: Maybe ConfigPaths
  , configExperimental :: Maybe ExperimentalConfigs
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

newtype ExperimentalConfigs = ExperimentalConfigs
  {gradle :: Maybe ExperimentalGradleConfigs}
  deriving (Eq, Ord, Show)

newtype ExperimentalGradleConfigs = ExperimentalGradleConfigs
  {gradleConfigsOnly :: Set Text}
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
      <*> obj .:? "experimental"

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

instance FromJSON ExperimentalConfigs where
  parseJSON = withObject "ExperimentalConfigs" $ \obj ->
    ExperimentalConfigs <$> obj .:? "gradle"

instance FromJSON ExperimentalGradleConfigs where
  parseJSON = withObject "ExperimentalGradleConfigs" $ \obj ->
    ExperimentalGradleConfigs <$> (obj .: "configurations-only" .!= Set.fromList [])
