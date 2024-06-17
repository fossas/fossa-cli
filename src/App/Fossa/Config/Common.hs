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
  parsePolicyOptions,
  configFileOpt,
  endpointOpt,
  apiKeyOpt,

  -- * CLI Validators
  validateDir,
  validateFile,
  validateExists,
  validateApiKey,
  validateApiKeyGeneric,

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

  -- * Global Parser Help Message
  endpointHelp,
  fossaApiKeyHelp,
  configHelp,
  titleHelp,
  -- Deprecation
  applyReleaseGroupDeprecationWarning,
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
  ProjectMetadata (..),
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
  warn,
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
import Effect.Logger (Logger, logDebug, logInfo, renderIt, vsep)
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
import Options.Applicative.Builder (helpDoc)
import Options.Applicative.Help (AnsiStyle)
import Path (Abs, Dir, File, Path, Rel, SomeBase (..), parseRelDir)
import Path.Extra (SomePath (..))
import Path.IO (resolveDir', resolveFile')
import Prettyprinter (Doc, Pretty (pretty), annotate)
import Prettyprinter.Render.Terminal (Color (Green, Red), color)
import Style (applyFossaStyle, boldItalicized, coloredBoldItalicized, formatDoc, stringToHelpDoc)
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
    <$> optional (strOption (applyFossaStyle <> long "title" <> short 't' <> helpDoc titleHelp))
    <*> optional (strOption (applyFossaStyle <> long "project-url" <> short 'P' <> stringToHelpDoc "This repository's home page"))
    <*> optional (strOption (applyFossaStyle <> long "jira-project-key" <> short 'j' <> stringToHelpDoc "This repository's JIRA project key"))
    <*> optional (strOption (applyFossaStyle <> long "link" <> short 'L' <> stringToHelpDoc "A link to attach to the current build"))
    <*> optional (strOption (applyFossaStyle <> long "team" <> short 'T' <> stringToHelpDoc "This repository's team inside your organization"))
    <*> parsePolicyOptions
    <*> many (strOption (applyFossaStyle <> long "project-label" <> stringToHelpDoc "Assign up to 5 labels to the project"))
    <*> optional releaseGroupMetadataOpts

titleHelp :: Maybe (Doc AnsiStyle)
titleHelp =
  Just . formatDoc $
    vsep
      [ "The title of the FOSSA project"
      , boldItalicized "Default: " <> "The project name"
      ]

policy :: Parser Policy
policy = PolicyName <$> (strOption (applyFossaStyle <> long "policy" <> helpDoc policyHelp))

policyId :: Parser Policy
policyId =
  PolicyId
    <$> ( option
            (readMWithError "failed to parse --policy-id, expecting int")
            (applyFossaStyle <> long "policy-id" <> helpDoc policyIdHelp)
        )

parsePolicyOptions :: Parser (Maybe Policy)
parsePolicyOptions = optional (policy <|> policyId) -- For Parsers '<|>' tries every alternative and fails if they all succeed.

policyHelp :: Maybe (Doc AnsiStyle)
policyHelp =
  Just . formatDoc $
    vsep
      [ "The name of the policy to assign to this project in FOSSA. Mutually excludes " <> coloredBoldItalicized Green "--policy-id" <> "."
      ]
policyIdHelp :: Maybe (Doc AnsiStyle)
policyIdHelp =
  Just . formatDoc $
    vsep
      [ "The id of the policy to assign to this project in FOSSA. Mutually excludes " <> coloredBoldItalicized Green "--policy" <> "."
      ]

releaseGroupMetadataOpts :: Parser ReleaseGroupMetadata
releaseGroupMetadataOpts =
  ReleaseGroupMetadata
    <$> strOption (applyFossaStyle <> long "release-group-name" <> helpDoc releaseGroupNameHelp)
    <*> strOption (applyFossaStyle <> long "release-group-release" <> helpDoc releaseGroupReleaseHelp)

releaseGroupNameHelp :: Maybe (Doc AnsiStyle)
releaseGroupNameHelp =
  Just . formatDoc $
    vsep
      [ "The name of the release group to add this project to"
      , boldItalicized "Note: " <> pretty releaseGroupDeprecationMessage
      ]

releaseGroupReleaseHelp :: Maybe (Doc AnsiStyle)
releaseGroupReleaseHelp =
  Just . formatDoc $
    vsep
      [ "The release of the release group to add this project to"
      , boldItalicized "Note: " <> pretty releaseGroupDeprecationMessage
      ]

applyReleaseGroupDeprecationWarning :: Has Diagnostics sig m => ProjectMetadata -> m ()
applyReleaseGroupDeprecationWarning projectMetadata = do
  case (projectReleaseGroup projectMetadata) of
    Nothing -> pure ()
    Just _ -> do
      warn releaseGroupDeprecationMessage
      pure ()

releaseGroupDeprecationMessage :: Text
releaseGroupDeprecationMessage =
  renderIt $
    vsep
      [annotate (color Red) "Release group options for this command will soon be deprecated. Refer to `fossa release-group` subcommands to interact with FOSSA release groups."]

pathOpt :: String -> Either String (Path Rel Dir)
pathOpt = first show . parseRelDir

targetOpt :: String -> Either String TargetFilter
targetOpt = first errorBundlePretty . runParser targetFilterParser "(Command-line arguments)" . toText

baseDirArg :: Parser String
baseDirArg = argument str (applyFossaStyle <> metavar "DIR" <> helpDoc baseDirDoc <> value ".")
  where
    baseDirDoc =
      Just . formatDoc $
        vsep
          [ "Set the base directory for scanning"
          , boldItalicized "Default: " <> "Current directory"
          ]

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
      -- API key precedence is strictly defined:
      -- 1. Cmd-line option (rarely used, not encouraged)
      -- 2. Config file (maybe used)
      -- 3. Environment Variable (most common)
      optAPIKey
        <|> (maybeConfigFile >>= configApiKey)
        <|> envApiKey
  if Data.Text.null . strip $ textkey
    then fatalText "A FOSSA API key was specified, but it is an empty string"
    else pure $ ApiKey textkey

validateApiKeyGeneric ::
  ( Has Diagnostics sig m
  ) =>
  Maybe ConfigFile ->
  Maybe Text ->
  Maybe Text ->
  m ApiKey
validateApiKeyGeneric maybeConfigFile maybeEnvApiKey maybeOptAPIKey = do
  textkey <-
    fromMaybeText "A FOSSA API key is required to run this command" $
      -- API key precedence is strictly defined:
      -- 1. Cmd-line option (rarely used, not encouraged)
      -- 2. Config file (maybe used)
      -- 3. Environment Variable (most common)
      maybeOptAPIKey
        <|> (maybeConfigFile >>= configApiKey)
        <|> maybeEnvApiKey
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
            logDebug (renderFailure emittedWarns errGroup "Unable to infer project revision from VCS")
            logInfo "Unable to infer project revision from VCS, using current timestamp as the revision."
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
    <$> switch (applyFossaStyle <> long "debug" <> stringToHelpDoc "Enable debug logging, and write detailed debug information to `fossa.debug.json`")
    <*> endpointOpt
    <*> optional (strOption (applyFossaStyle <> long "project" <> short 'p' <> helpDoc projectHelp))
    <*> optional (strOption (applyFossaStyle <> long "revision" <> short 'r' <> helpDoc revisionHelp))
    <*> apiKeyOpt
    <*> configFileOpt
    <*> optional (option parseTelemetryScope (applyFossaStyle <> long "with-telemetry-scope" <> helpDoc telemtryScopeHelp))
  where
    projectHelp :: Maybe (Doc AnsiStyle)
    projectHelp =
      Just . formatDoc $
        vsep
          [ "This repository's URL or VCS endpoint"
          , boldItalicized "Default: " <> "VCS remote 'origin'"
          ]
    revisionHelp :: Maybe (Doc AnsiStyle)
    revisionHelp =
      Just . formatDoc $
        vsep
          [ "This repository's current revision hash"
          , boldItalicized "Default: " <> "VCS hash HEAD"
          ]

    telemtryScopeHelp :: Maybe (Doc AnsiStyle)
    telemtryScopeHelp =
      Just . formatDoc $
        vsep
          [ "Scope of telemetry to use"
          , boldItalicized "Options: " <> coloredBoldItalicized Green "full" <> boldItalicized "|" <> coloredBoldItalicized Green "off"
          , boldItalicized "Default: " <> coloredBoldItalicized Green "full"
          ]

apiKeyOpt :: Parser (Maybe Text)
apiKeyOpt = optional (strOption (applyFossaStyle <> long fossaApiKeyCmdText <> helpDoc fossaApiKeyHelp))

endpointOpt :: Parser (Maybe URI)
endpointOpt = optional (uriOption (applyFossaStyle <> long "endpoint" <> short 'e' <> metavar "URL" <> helpDoc endpointHelp))

configFileOpt :: Parser (Maybe FilePath)
configFileOpt = optional (strOption (applyFossaStyle <> long "config" <> short 'c' <> helpDoc configHelp))

configHelp :: Maybe (Doc AnsiStyle)
configHelp =
  Just . formatDoc $
    vsep
      [ "Path to configuration file including filename"
      , boldItalicized "Default: " <> ".fossa.yml"
      ]

endpointHelp :: Maybe (Doc AnsiStyle)
endpointHelp =
  Just . formatDoc $
    vsep
      [ "The FOSSA API server base URL"
      , boldItalicized "Default: " <> "https://platform.fossa.com"
      ]

fossaApiKeyHelp :: Maybe (Doc AnsiStyle)
fossaApiKeyHelp =
  Just . formatDoc $
    vsep
      [ "The FOSSA API server authentication key"
      , boldItalicized "Default: " <> "FOSSA_API_KEY from env"
      ]

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
