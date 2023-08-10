{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Config.ConfigFile (
  defaultConfigFileNames,
  resolveConfigFile,
  ConfigGrepEntry (..),
  ConfigFile (..),
  ConfigProject (..),
  ConfigRevision (..),
  ConfigTargets (..),
  ConfigPaths (..),
  ConfigTelemetry (..),
  ConfigTelemetryScope (..),
  ExperimentalConfigs (..),
  ExperimentalGradleConfigs (..),
  VendoredDependencyConfigs (..),
  mergeFileCmdMetadata,
  empty,
  resolveLocalConfigFile,
) where

import App.Docs (fossaYmlDocUrl)
import App.Types (Policy (..), ProjectMetadata (..), ReleaseGroupMetadata)
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
  withText,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (ToString (toString), ToText (toText))
import Data.Text (Text, strip, toLower)
import Effect.Logger (
  AnsiStyle,
  Doc,
  Logger,
  Pretty (pretty),
  logDebug,
  logWarn,
  viaShow,
  vsep,
 )
import Effect.ReadFS (ReadFS, doesFileExist, getCurrentDir, readContentsYaml)
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
import Types (ArchiveUploadType, LicenseScanPathFilters, TargetFilter)

defaultConfigFileNames :: [Path Rel File]
defaultConfigFileNames =
  [ $(mkRelFile ".fossa.yml")
  , $(mkRelFile ".fossa.yaml")
  ]

data ConfigLocation
  = DefaultConfigLocation (Path Abs Dir)
  | SpecifiedConfigLocation (Path Abs File)

resolveLocalConfigFile ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  Maybe FilePath ->
  m (Maybe ConfigFile)
resolveLocalConfigFile path = do
  cwd <- getCurrentDir
  resolveConfigFile cwd path

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
  logDebug $ "Loading configuration file from " <> viaShow base
  loc <- resolveLocation base path
  case loc of
    DefaultConfigLocation dir -> do
      possibleConfigFilePath <-
        asum
          <$> traverse
            ( \configFileName -> do
                let configFilePath = dir </> configFileName
                exists <- doesFileExist configFilePath
                pure $ if exists then Just configFilePath else Nothing
            )
            defaultConfigFileNames
      case possibleConfigFilePath of
        Just actualConfigFilePath -> do
          configFile <- readContentsYaml actualConfigFilePath
          let version = configVersion configFile
          if version >= 3
            then pure $ Just configFile
            else -- Invalid config found without --config flag: warn and ignore file.
              logWarn (warnMsgForOlderConfig @AnsiStyle version) $> Nothing
        Nothing -> pure Nothing
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

mergeFileCmdMetadata :: Has Diagnostics sig m => ProjectMetadata -> ConfigFile -> m ProjectMetadata
mergeFileCmdMetadata meta cfgFile =
  case (metaPolicy, cfgFilePolicy) of
    (Just (PolicyId _), Just (PolicyName _)) -> err
    (Just (PolicyName _), Just (PolicyId _)) -> err
    _ -> pure . mkMeta $ metaPolicy <|> cfgFilePolicy
  where
    err = fatalText "Only one of policy or policyId can be set. Check your cli options and .fossa.yml to ensure you aren't specifying both."
    metaPolicy = projectPolicy meta
    cfgFilePolicy = configProject cfgFile >>= configPolicy

    mkMeta :: Maybe Policy -> ProjectMetadata
    mkMeta policy =
      ProjectMetadata
        { projectTitle = projectTitle meta <|> (configProject cfgFile >>= configName)
        , projectUrl = projectUrl meta <|> (configProject cfgFile >>= configUrl)
        , projectJiraKey = projectJiraKey meta <|> (configProject cfgFile >>= configJiraKey)
        , projectLink = projectLink meta <|> (configProject cfgFile >>= configLink)
        , projectTeam = projectTeam meta <|> (configProject cfgFile >>= configTeam)
        , projectPolicy = policy
        , projectLabel = projectLabel meta <|> (maybe [] configLabel (configProject cfgFile))
        , projectReleaseGroup = projectReleaseGroup meta <|> (configProject cfgFile >>= configReleaseGroup)
        }

empty :: ConfigFile
empty = ConfigFile 3 Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data ConfigFile = ConfigFile
  { configVersion :: Int
  , configServer :: Maybe Text
  , configApiKey :: Maybe Text
  , configProject :: Maybe ConfigProject
  , configRevision :: Maybe ConfigRevision
  , configTargets :: Maybe ConfigTargets
  , configPaths :: Maybe ConfigPaths
  , configExperimental :: Maybe ExperimentalConfigs
  , configVendoredDependencies :: Maybe VendoredDependencyConfigs
  , configTelemetry :: Maybe ConfigTelemetry
  , configCustomLicenseSearch :: Maybe (NonEmpty ConfigGrepEntry)
  , configKeywordSearch :: Maybe (NonEmpty ConfigGrepEntry)
  }
  deriving (Eq, Ord, Show)

data ConfigProject = ConfigProject
  { configProjID :: Maybe Text
  , configName :: Maybe Text
  , configLink :: Maybe Text
  , configTeam :: Maybe Text
  , configJiraKey :: Maybe Text
  , configUrl :: Maybe Text
  , configPolicy :: Maybe Policy
  , configLabel :: [Text]
  , configReleaseGroup :: Maybe ReleaseGroupMetadata
  , configPolicyId :: Maybe Int
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

data ConfigGrepEntry = ConfigGrepEntry
  { configGrepMatchCriteria :: Text
  , configGrepName :: Text
  }
  deriving (Eq, Ord, Show)

newtype ConfigTelemetry = ConfigTelemetry
  { telemetryScope :: ConfigTelemetryScope
  }
  deriving (Eq, Ord, Show)

data ConfigTelemetryScope
  = NoTelemetry
  | FullTelemetry
  deriving (Eq, Ord, Show)

newtype ExperimentalConfigs = ExperimentalConfigs
  {gradle :: Maybe ExperimentalGradleConfigs}
  deriving (Eq, Ord, Show)

newtype ExperimentalGradleConfigs = ExperimentalGradleConfigs
  {gradleConfigsOnly :: Set Text}
  deriving (Eq, Ord, Show)

instance FromJSON ConfigFile where
  parseJSON = withObject "ConfigFile" $ \obj ->
    ConfigFile
      <$> obj .: "version"
      <*> obj .:? "server"
      <*> obj .:? "apiKey"
      <*> obj .:? "project"
      <*> obj .:? "revision"
      <*> obj .:? "targets"
      <*> obj .:? "paths"
      <*> obj .:? "experimental"
      <*> obj .:? "vendoredDependencies"
      <*> obj .:? "telemetry"
      <*> obj .:? "customLicenseSearch"
      <*> obj .:? "experimentalKeywordSearch"

instance FromJSON ConfigProject where
  parseJSON = withObject "ConfigProject" $ \obj ->
    ConfigProject
      <$> obj .:? "id"
      <*> obj .:? "name"
      <*> obj .:? "link"
      <*> obj .:? "team"
      <*> obj .:? "jiraProjectKey"
      <*> obj .:? "url"
      <*> parsePolicy obj
      <*> obj .:? "labels" .!= []
      <*> obj .:? "releaseGroup"
      <*> obj .:? "policyId"
    where
      parsePolicy obj = do
        pName <- obj .:? "policy"
        pId <- obj .:? "policyId"
        case (pName, pId) of
          (Just _, Just _) -> fail "Only one of 'policy' or 'policyId' can be set at a time."
          _ -> pure $ (PolicyName <$> pName) <|> (PolicyId <$> pId)

instance FromJSON ConfigRevision where
  parseJSON = withObject "ConfigRevision" $ \obj ->
    ConfigRevision
      <$> obj .:? "commit"
      <*> obj .:? "branch"

instance FromJSON ConfigTargets where
  parseJSON = withObject "ConfigTargets" $ \obj ->
    ConfigTargets
      <$> (obj .:? "only" .!= [])
      <*> (obj .:? "exclude" .!= [])

instance FromJSON ConfigPaths where
  parseJSON = withObject "ConfigPaths" $ \obj ->
    ConfigPaths
      <$> (obj .:? "only" .!= [])
      <*> (obj .:? "exclude" .!= [])

instance FromJSON ExperimentalConfigs where
  parseJSON = withObject "ExperimentalConfigs" $ \obj ->
    ExperimentalConfigs <$> obj .:? "gradle"

instance FromJSON ExperimentalGradleConfigs where
  parseJSON = withObject "ExperimentalGradleConfigs" $ \obj ->
    ExperimentalGradleConfigs <$> (obj .: "configurations-only" .!= Set.fromList [])

instance FromJSON ConfigTelemetry where
  parseJSON = withObject "ConfigTelemetry" $ \obj ->
    ConfigTelemetry <$> (obj .: "scope")

instance FromJSON ConfigGrepEntry where
  parseJSON = withObject "ConfigGrepEntry" $ \obj ->
    ConfigGrepEntry
      <$> (obj .: "matchCriteria")
      <*> (obj .: "name")

instance FromJSON ConfigTelemetryScope where
  parseJSON = withText "ConfigTelemetryScope" $ \scope ->
    case toLower . strip $ scope of
      "full" -> pure FullTelemetry
      "off" -> pure NoTelemetry
      notSupported -> fail . toString $ "Expected either: 'full' or 'off' for telemetry scope. You provided: " <> notSupported

data VendoredDependencyConfigs = VendoredDependencyConfigs
  { configForceRescans :: Bool
  , configLicenseScanMethod :: Maybe ArchiveUploadType
  , configLicenseScanPathFilters :: Maybe LicenseScanPathFilters
  }
  deriving (Eq, Ord, Show)

instance FromJSON VendoredDependencyConfigs where
  parseJSON = withObject "vendoredDependencies" $ \obj ->
    VendoredDependencyConfigs
      <$> (obj .:? "forceRescans" .!= False)
      <*> (obj .:? "scanMethod")
      <*> (obj .:? "licenseScanPathFilters")
