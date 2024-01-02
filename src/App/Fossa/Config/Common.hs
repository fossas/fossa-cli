{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Common (
  -- * CLI Parsers
  CommonOpts (..),
  commonOpts,
  releaseGroupMetadataOpts,
  pathOpt,
  targetOpt,
  baseDirArg,
  metadataOpts,

  -- * CLI Validators
  validateDir,
  validateFile,
  validateExists,
  validateApiKey,

  -- * CLI Collectors
  collectBaseDir,
  collectRevisionData,
  CacheAction (..),
  collectRevisionOverride,
  collectAPIMetadata,
  collectApiOpts,
  collectTelemetrySink,
  collectConfigFileFilters,
  collectConfigMavenScopeFilters,

  -- * Configuration Types
  ScanDestination (..),

  -- * Global Defaults
  defaultTimeoutDuration,
  collectRevisionData',
  fossaApiKeyCmdText,
) where

import App.Fossa.Config.ConfigFile (
  ConfigFile (
    configApiKey,
    configMavenScope,
    configPaths,
    configProject,
    configRevision,
    configServer,
    configTargets,
    configTelemetry
  ),
  ConfigPaths (pathsExclude, pathsOnly),
  ConfigProject (configProjID),
  ConfigRevision (configBranch, configCommit),
  ConfigTargets (targetsExclude, targetsOnly),
  ConfigTelemetry (telemetryScope),
  ConfigTelemetryScope (..),
  MavenScopeConfig (..),
  mergeFileCmdMetadata,
 )
import App.Fossa.Config.EnvironmentVars (EnvVars (..))
import App.Fossa.ProjectInference (
  InferredProject,
  inferProjectCached,
  inferProjectDefault,
  inferProjectFromVCS,
  mergeOverride,
  saveRevision,
 )
import App.OptionExtensions (uriOption)
import App.Types (
  BaseDir (BaseDir),
  OverrideProject (..),
  Policy (..),
  ProjectMetadata (ProjectMetadata),
  ProjectRevision,
  ReleaseGroupMetadata (ReleaseGroupMetadata),
 )
import Control.Carrier.Telemetry.Types (
  TelemetrySink (TelemetrySinkToEndpoint, TelemetrySinkToFile),
 )
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  errCtx,
  errorBoundary,
  fatalText,
  fromMaybeText,
  recover,
  rethrow,
  (<||>),
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Timeout (Duration (Minutes))
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Bifunctor (Bifunctor (first))
import Data.Functor.Extra ((<$$>))
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.String.Conversion (ToText (toText))
import Data.Text (Text, null, strip, toLower)
import Diag.Result (Result (Failure, Success), renderFailure)
import Discovery.Filters (AllFilters (AllFilters), MavenScopeFilters (..), comboExclude, comboInclude, setExclude, setInclude, targetFilterParser)
import Effect.Exec (Exec)
import Effect.Logger (Logger, logDebug, logInfo)
import Effect.ReadFS (ReadFS, doesDirExist, doesFileExist)
import Fossa.API.Types (ApiKey (ApiKey), ApiOpts (ApiOpts), defaultApiPollDelay)
import GHC.Generics (Generic)
import Options.Applicative (
  Alternative (many),
  Parser,
  ReadM,
  argument,
  auto,
  eitherReader,
  help,
  long,
  metavar,
  option,
  optional,
  readerError,
  short,
  str,
  strOption,
  switch,
  value,
  (<|>),
 )
import Path (Abs, Dir, File, Path, Rel, SomeBase (..), parseRelDir)
import Path.Extra (SomePath (..))
import Path.IO (resolveDir', resolveFile')
import Text.Megaparsec (errorBundlePretty, runParser)
import Text.URI (URI, mkURI)
import Types (TargetFilter)

data ScanDestination
  = -- | upload to fossa with provided api key and base url
    UploadScan ApiOpts ProjectMetadata
  | OutputStdout
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ScanDestination where
  toEncoding = genericToEncoding defaultOptions

data CacheAction
  = ReadOnly
  | WriteOnly
  deriving (Eq, Ord, Show)

defaultTimeoutDuration :: Duration
defaultTimeoutDuration = Minutes 60

readMWithError :: Read a => String -> ReadM a
readMWithError errMsg = auto <|> readerError errMsg

metadataOpts :: Parser ProjectMetadata
metadataOpts =
  ProjectMetadata
    <$> optional (strOption (long "title" <> short 't' <> help "the title of the FOSSA project. (default: the project name)"))
    <*> optional (strOption (long "project-url" <> short 'P' <> help "this repository's home page"))
    <*> optional (strOption (long "jira-project-key" <> short 'j' <> help "this repository's JIRA project key"))
    <*> optional (strOption (long "link" <> short 'L' <> help "a link to attach to the current build"))
    <*> optional (strOption (long "team" <> short 'T' <> help "this repository's team inside your organization"))
    <*> parsePolicyOptions
    <*> many (strOption (long "project-label" <> help "assign up to 5 labels to the project"))
    <*> optional releaseGroupMetadataOpts
  where
    policy :: Parser Policy
    policy = PolicyName <$> (strOption (long "policy" <> help "The name of the policy to assign to this project in FOSSA. Mutually excludes --policy-id."))

    policyId :: Parser Policy
    policyId =
      PolicyId
        <$> ( option
                (readMWithError "failed to parse --policy-id, expecting int")
                (long "policy-id" <> help "The id of the policy to assign to this project in FOSSA. Mutually excludes --policy.")
            )

    parsePolicyOptions :: Parser (Maybe Policy)
    parsePolicyOptions = optional (policy <|> policyId) -- For Parsers '<|>' tries every alternative and fails if they all succeed.

releaseGroupMetadataOpts :: Parser ReleaseGroupMetadata
releaseGroupMetadataOpts =
  ReleaseGroupMetadata
    <$> strOption (long "release-group-name" <> help "the name of the release group to add this project to")
    <*> strOption (long "release-group-release" <> help "the release of the release group to add this project to")

pathOpt :: String -> Either String (Path Rel Dir)
pathOpt = first show . parseRelDir

targetOpt :: String -> Either String TargetFilter
targetOpt = first errorBundlePretty . runParser targetFilterParser "(Command-line arguments)" . toText

baseDirArg :: Parser String
baseDirArg = argument str (metavar "DIR" <> help "Set the base directory for scanning (default: current directory)" <> value ".")

collectBaseDir ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  FilePath ->
  m BaseDir
collectBaseDir = fmap BaseDir . validateDir

validateDir ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  FilePath ->
  m (Path Abs Dir)
validateDir fp = do
  dir <- sendIO $ resolveDir' fp
  exists <- doesDirExist dir
  if exists
    then pure dir
    else fatalText $ "Directory does not exist: " <> toText dir

validateFile ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  FilePath ->
  m (Path Abs File)
validateFile fp = do
  file <- sendIO $ resolveFile' fp
  exists <- doesFileExist file
  if exists
    then pure file
    else fatalText $ "File does not exist: " <> toText file

validateExists ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  FilePath ->
  m SomePath
validateExists fp =
  recover (validateDir fp) >>= \case
    Nothing ->
      recover (validateFile fp) >>= \case
        Nothing -> fatalText $ "File does not exist: " <> toText fp
        -- Type included here so that if @validateFile@ ever changes this fails to compile,
        -- since we're asserting @Abs@.
        Just (resolved :: Path Abs File) -> pure . SomeFile $ Abs resolved
    -- Type included here so that if @validateFile@ ever changes this fails to compile,
    -- since we're asserting @Abs@.
    Just (resolved :: Path Abs Dir) -> pure . SomeDir $ Abs resolved

validateApiKey ::
  ( Has Diagnostics sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  CommonOpts ->
  m ApiKey
validateApiKey maybeConfigFile EnvVars{envApiKey} CommonOpts{optAPIKey} = do
  textkey <-
    fromMaybeText "A FOSSA API key is required to run this command" $
      -- API key significance is strictly defined:
      -- 1. Cmd-line option (rarely used, not encouraged)
      -- 2. Config file (maybe used)
      -- 3. Environment Variable (most common)
      optAPIKey
        <|> (maybeConfigFile >>= configApiKey)
        <|> envApiKey
  if Data.Text.null . strip $ textkey
    then fatalText "A FOSSA API key was specified, but it is an empty string"
    else pure $ ApiKey textkey

collectApiOpts :: (Has Diagnostics sig m) => Maybe ConfigFile -> EnvVars -> CommonOpts -> m ApiOpts
collectApiOpts maybeconfig envvars globals = do
  apikey <- validateApiKey maybeconfig envvars globals
  let configUri = maybeconfig >>= configServer >>= mkURI
      baseuri = optBaseUrl globals <|> configUri
  pure $ ApiOpts baseuri apikey defaultApiPollDelay

collectRevisionOverride :: Maybe ConfigFile -> OverrideProject -> OverrideProject
collectRevisionOverride maybeConfig OverrideProject{..} = override
  where
    override = OverrideProject projectName projectRevision projectBranch

    projectName :: Maybe Text
    projectName = overrideName <|> (maybeConfig >>= configProject >>= configProjID)

    projectRevision :: Maybe Text
    projectRevision = overrideRevision <|> (maybeConfig >>= configRevision >>= configCommit)

    projectBranch :: Maybe Text
    projectBranch = overrideBranch <|> (maybeConfig >>= configRevision >>= configBranch)

-- | Handles reading from and writing to the revision cache, based on CacheAction.
collectRevisionData ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  BaseDir ->
  Maybe ConfigFile ->
  CacheAction ->
  OverrideProject ->
  m ProjectRevision
collectRevisionData (BaseDir basedir) maybeConfig cacheStrategy cliOverride = do
  let override = collectRevisionOverride maybeConfig cliOverride
  case cacheStrategy of
    ReadOnly -> do
      inferred <- inferVCSInfo $ inferProjectCached basedir <||> inferProjectDefault basedir
      pure $ mergeOverride override inferred
    WriteOnly -> do
      inferred <- inferVCSInfo $ inferProjectDefault basedir
      let revision = mergeOverride override inferred
      saveRevision revision
      pure revision
  where
    inferVCSInfo ::
      ( Has Diagnostics sig m
      , Has Logger sig m
      , Has ReadFS sig m
      , Has Exec sig m
      ) =>
      m InferredProject ->
      m InferredProject
    inferVCSInfo nextStep = do
      vcsInfo <- errorBoundary $ inferProjectFromVCS basedir
      case vcsInfo of
        Failure emittedWarns errGroup ->
          do
            logDebug (renderFailure emittedWarns errGroup "")
            logInfo "Unable to infer project revision from version control system. The project revision will be set to the current timestamp."
            nextStep
        Success _ _ -> rethrow vcsInfo

collectRevisionData' ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  m BaseDir ->
  Maybe ConfigFile ->
  CacheAction ->
  OverrideProject ->
  m ProjectRevision
collectRevisionData' basedir cfg cache override = do
  basedir' <- errCtx ("Cannot collect revision data without a valid base directory" :: Text) basedir
  collectRevisionData basedir' cfg cache override

collectAPIMetadata :: Has Diagnostics sig m => Maybe ConfigFile -> ProjectMetadata -> m ProjectMetadata
collectAPIMetadata cfgfile cliMeta = maybe (pure cliMeta) (mergeFileCmdMetadata cliMeta) cfgfile

collectTelemetrySink :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Maybe ConfigFile -> EnvVars -> Maybe CommonOpts -> m (Maybe TelemetrySink)
collectTelemetrySink maybeConfigFile envvars maybeOpts = do
  let defaultScope = FullTelemetry
  -- Precedence is
  --  (1) command line
  --  (2) environment variable
  --  (3) configuration file
  let providedScope =
        fromMaybe
          defaultScope
          ( (maybeOpts >>= optTelemetry)
              <|> (envTelemetryScope envvars)
              <|> (telemetryScope <$> (configTelemetry =<< maybeConfigFile))
          )

  let isDebugMode = envTelemetryDebug envvars || (fmap optDebug maybeOpts == Just True)
  case (isDebugMode, providedScope) of
    (True, FullTelemetry) -> pure $ Just TelemetrySinkToFile
    (True, NoTelemetry) -> pure Nothing
    (False, NoTelemetry) -> pure Nothing
    (False, FullTelemetry) -> do
      let candidateOpts = fromMaybe emptyCommonOpts maybeOpts

      -- Not all commands require api key, if we do not have valid api key
      -- we do not know which endpoint to sink telemetry, this ensures we
      -- we do not emit telemetry of on-prem users when they do not have
      -- api opts configured.
      TelemetrySinkToEndpoint <$$> recover (collectApiOpts maybeConfigFile envvars candidateOpts)

data CommonOpts = CommonOpts
  { optDebug :: Bool
  , optBaseUrl :: Maybe URI
  , optProjectName :: Maybe Text
  , optProjectRevision :: Maybe Text
  , optAPIKey :: Maybe Text
  , optConfig :: Maybe FilePath
  , optTelemetry :: Maybe ConfigTelemetryScope
  }
  deriving (Eq, Ord, Show)

emptyCommonOpts :: CommonOpts
emptyCommonOpts = CommonOpts False Nothing Nothing Nothing Nothing Nothing Nothing

parseTelemetryScope :: ReadM ConfigTelemetryScope
parseTelemetryScope = eitherReader $ \scope ->
  case toLower . strip . toText $ scope of
    "off" -> Right NoTelemetry
    "full" -> Right FullTelemetry
    _ -> Left "Failed to parse telemetry scope, expected either: full or off"

fossaApiKeyCmdText :: IsString a => a
fossaApiKeyCmdText = "fossa-api-key"

commonOpts :: Parser CommonOpts
commonOpts =
  CommonOpts
    <$> switch (long "debug" <> help "Enable debug logging, and write detailed debug information to `fossa.debug.json`")
    <*> optional (uriOption (long "endpoint" <> short 'e' <> metavar "URL" <> help "The FOSSA API server base URL (default: https://app.fossa.com)"))
    <*> optional (strOption (long "project" <> short 'p' <> help "this repository's URL or VCS endpoint (default: VCS remote 'origin')"))
    <*> optional (strOption (long "revision" <> short 'r' <> help "this repository's current revision hash (default: VCS hash HEAD)"))
    <*> optional (strOption (long fossaApiKeyCmdText <> help "the FOSSA API server authentication key (default: FOSSA_API_KEY from env)"))
    <*> optional (strOption (long "config" <> short 'c' <> help "Path to configuration file including filename (default: .fossa.yml)"))
    <*> optional (option parseTelemetryScope (long "with-telemetry-scope" <> help "Scope of telemetry to use, the options are 'full' or 'off'. (default: 'full')"))

collectConfigFileFilters :: ConfigFile -> AllFilters
collectConfigFileFilters configFile = do
  let pullFromFile :: (a -> [b]) -> (ConfigFile -> Maybe a) -> [b]
      pullFromFile field section = maybe [] field (section configFile)
      onlyT = pullFromFile targetsOnly configTargets
      onlyP = pullFromFile pathsOnly configPaths
      excludeT = pullFromFile targetsExclude configTargets
      excludeP = pullFromFile pathsExclude configPaths

  AllFilters (comboInclude onlyT onlyP) (comboExclude excludeT excludeP)

collectConfigMavenScopeFilters :: ConfigFile -> MavenScopeFilters
collectConfigMavenScopeFilters configFile = do
  let maybeMavenScopeConfigs = configMavenScope configFile
  case maybeMavenScopeConfigs of
    Nothing -> MavenScopeIncludeFilters mempty
    Just mavenScopeConfig -> case mavenScopeConfig of
      MavenScopeOnlyConfig filters -> MavenScopeIncludeFilters $ setInclude filters
      MavenScopeExcludeConfig filters -> MavenScopeExcludeFilters $ setExclude filters
