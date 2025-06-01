{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Config.ConfigFile (
  defaultConfigFileNames,
  resolveConfigFile,
  OrgWideCustomLicenseConfigPolicy (..),
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
  MavenScopeConfig (..),
  ReachabilityConfigFile (..),
  ConfigReleaseGroup (..),
  ConfigReleaseGroupProject (..),
  mergeFileCmdMetadata,
  resolveLocalConfigFile,
) where

import App.Docs (fossaYmlDocUrl)
import App.Fossa.Ficus.Types (OrgWideCustomLicenseConfigPolicy (..))
import App.Types (Policy (..), ProjectMetadata (..), ReleaseGroupMetadata)
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  ToDiagnostic,
  context,
  fatal,
  fatalText,
  fromEitherShow,
 )
import Control.Effect.Lift (Lift)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON,
  withObject,
  withText,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Error (createBody, renderErrataStack)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (ToString (toString), ToText (toText))
import Data.Text (Text, strip, toLower)
import Diag.Diagnostic (ToDiagnostic (..))
import Effect.Logger (
  Logger,
  logDebug,
  logWarn,
  viaShow,
 )
import Effect.ReadFS (ReadFS, doesFileExist, getCurrentDir, readContentsYaml)
import Errata (Errata (..))
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
          configFile <- ($ actualConfigFilePath) <$> readContentsYaml actualConfigFilePath
          let version = configVersion configFile
          if version >= 3
            then pure $ Just configFile
            else -- Invalid config found without --config flag: warn and ignore file.
              logWarn (renderErrataStack [renderDiagnostic $ OlderConfigError version]) $> Nothing
        Nothing -> pure Nothing
    SpecifiedConfigLocation realpath -> do
      exists <- doesFileExist realpath
      if not exists
        then -- file requested, but missing
          fatalText ("requested config file does not exist: " <> toText realpath)
        else do
          configFile <- ($ realpath) <$> readContentsYaml realpath
          let version = configVersion configFile
          if version >= 3
            then pure $ Just configFile
            else -- Invalid config with --config specified: fail with message.
              fatal $ OlderConfigError version

newtype OlderConfigError = OlderConfigError Int
instance ToDiagnostic OlderConfigError where
  renderDiagnostic (OlderConfigError foundVersion) = do
    let header = "Incompatible [.fossa.yml] found"
        ctx = "Expecting `version: 3`; found `version: " <> toText foundVersion <> "`"
        body = createBody Nothing (Just fossaYmlDocUrl) Nothing Nothing (Just ctx)
    Errata (Just header) [] (Just body)

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
        , projectReleaseGroup = projectReleaseGroup meta <|> (configProject cfgFile >>= configProjectReleaseGroup)
        }

data ConfigFile = ConfigFile
  { configVersion :: Int
  , configServer :: Maybe Text
  , configApiKey :: Maybe Text
  , configReleaseGroup :: Maybe ConfigReleaseGroup
  , configProject :: Maybe ConfigProject
  , configRevision :: Maybe ConfigRevision
  , configTargets :: Maybe ConfigTargets
  , configPaths :: Maybe ConfigPaths
  , configExperimental :: Maybe ExperimentalConfigs
  , configMavenScope :: Maybe MavenScopeConfig
  , configVendoredDependencies :: Maybe VendoredDependencyConfigs
  , configTelemetry :: Maybe ConfigTelemetry
  , configCustomLicenseSearch :: Maybe [ConfigGrepEntry]
  , configKeywordSearch :: Maybe [ConfigGrepEntry]
  , configReachability :: Maybe ReachabilityConfigFile
  , configOrgWideCustomLicenseConfigPolicy :: OrgWideCustomLicenseConfigPolicy
  , configConfigFilePath :: Path Abs File
  }
  deriving (Eq, Ord, Show)

data ConfigProject = ConfigProject
  { configProjLocator :: Maybe Text
  , configProjID :: Maybe Text
  , configName :: Maybe Text
  , configLink :: Maybe Text
  , configTeam :: Maybe Text
  , configTeams :: Maybe [Text]
  , configJiraKey :: Maybe Text
  , configUrl :: Maybe Text
  , configPolicy :: Maybe Policy
  , configLabel :: [Text]
  , configProjectReleaseGroup :: Maybe ReleaseGroupMetadata
  , configPolicyId :: Maybe Int
  }
  deriving (Eq, Ord, Show)

data ConfigReleaseGroup = ConfigReleaseGroup
  { configReleaseGroupTitle :: Maybe Text
  , configReleaseGroupRelease :: Maybe Text
  , configReleaseGroupProjects :: Maybe [ConfigReleaseGroupProject]
  , configReleaseGroupLicensePolicy :: Maybe Text
  , configReleaseGroupSecurityPolicy :: Maybe Text
  , configReleaseGroupQualityPolicy :: Maybe Text
  , configReleaseGroupTeams :: Maybe [Text]
  }
  deriving (Eq, Ord, Show)

data ConfigReleaseGroupProject = ConfigReleaseGroupProject
  { configReleaseGroupProjectId :: Text
  , configReleaseGroupProjectRevision :: Text
  , configReleaseGroupProjectBranch :: Text
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
  { configGrepName :: Text
  , configGrepMatchCriteria :: Text
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

data MavenScopeConfig = MavenScopeOnlyConfig (Set Text) | MavenScopeExcludeConfig (Set Text)
  deriving (Eq, Ord, Show)

instance FromJSON (Path Abs File -> ConfigFile) where
  parseJSON = withObject "ConfigFile" $ \obj ->
    ConfigFile
      <$> obj .: "version"
      <*> obj .:? "server"
      <*> obj .:? "apiKey"
      <*> obj .:? "releaseGroup"
      <*> obj .:? "project"
      <*> obj .:? "revision"
      <*> obj .:? "targets"
      <*> obj .:? "paths"
      <*> obj .:? "experimental"
      <*> obj .:? "maven"
      <*> obj .:? "vendoredDependencies"
      <*> obj .:? "telemetry"
      <*> obj .:? "customLicenseSearch"
      <*> obj .:? "experimentalKeywordSearch"
      <*> obj .:? "reachability"
      <*> parseIgnoreOrgWideCustomLicenseScanConfigs obj
    where
      parseIgnoreOrgWideCustomLicenseScanConfigs obj = do
        ignoreIt <- obj .:? "orgWideCustomLicenseScanConfigPolicy" .!= False
        if ignoreIt then pure Ignore else pure Use

instance FromJSON ConfigProject where
  parseJSON = withObject "ConfigProject" $ \obj ->
    ConfigProject
      <$> obj .:? "locator"
      <*> obj .:? "id"
      <*> obj .:? "name"
      <*> obj .:? "link"
      <*> obj .:? "team"
      <*> obj .:? "teams"
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

instance FromJSON MavenScopeConfig where
  parseJSON = withObject "MavenScopeConfig" $ \obj ->
    MavenScopeOnlyConfig . Set.fromList <$> (obj .: "scope-only" .!= [])
      <|> MavenScopeExcludeConfig . Set.fromList <$> (obj .:? "scope-exclude" .!= [])

instance FromJSON ConfigTelemetry where
  parseJSON = withObject "ConfigTelemetry" $ \obj ->
    ConfigTelemetry <$> (obj .: "scope")

instance FromJSON ConfigGrepEntry where
  parseJSON = withObject "ConfigGrepEntry" $ \obj ->
    ConfigGrepEntry
      <$> (obj .: "name")
      <*> (obj .: "matchCriteria")

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

-- | Configuration for reachability analysis.
newtype ReachabilityConfigFile = ReachabilityConfigFile
  { configFileReachabilityJvmOutputs :: Map String [String]
  -- ^ Reachability on JVM projects relies on analyzing the binary JAR files
  -- emitted by the build process. FOSSA CLI tries to identify these from project metadata,
  -- but this may not work for all projects.
  --
  -- The intention of this config is to allow users to specify their own locations
  -- in these situations.
  --
  -- The key is the project path (found via @fossa list-targets@)
  -- and the value is the list of JAR files built by that project.
  --
  -- Example:
  --
  -- > ; fossa list-targets
  -- > Found project: maven@./
  -- > Found target: maven@./:com.example.app:example-artifact
  --
  -- The config map should look like this when associating JARs to the project at @./@:
  --
  -- > {
  -- >   "./": [
  -- >     "some/other/example-artifact.jar",
  -- >     "yet/another/example-artifact.jar",
  -- >   ]
  -- > }
  --
  -- In particular, note that the @target@ entry is ignored.
  -- This config is only concerned with targets.
  }
  deriving (Eq, Ord, Show, Monoid, Semigroup, ToJSON)

instance FromJSON ReachabilityConfigFile where
  parseJSON = withObject "ReachabilityConfig" $ \obj ->
    ReachabilityConfigFile
      <$> (obj .:? "jvmOutputs" .!= Map.empty)

instance FromJSON ConfigReleaseGroup where
  parseJSON = withObject "ConfigReleaseGroup" $ \obj ->
    ConfigReleaseGroup
      <$> obj .:? "title"
      <*> obj .:? "release"
      <*> obj .:? "releaseGroupProjects"
      <*> obj .:? "licensePolicy"
      <*> obj .:? "securityPolicy"
      <*> obj .:? "qualityPolicy"
      <*> obj .:? "teams"

instance FromJSON ConfigReleaseGroupProject where
  parseJSON = withObject "ConfigReleaseGroupProject" $ \obj ->
    ConfigReleaseGroupProject
      <$> (obj .: "projectLocator")
      <*> (obj .: "projectRevision")
      <*> (obj .: "projectBranch")
