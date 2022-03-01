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
  validateApiKey,

  -- * CLI Collectors
  collectBaseDir,
  collectRevisionData,
  CacheAction (..),
  collectRevisionOverride,
  collectAPIMetadata,
  collectApiOpts,

  -- * Configuration Types
  ScanDestination (..),

  -- * Global Defaults
  defaultTimeoutDuration,
  collectRevisionData',
) where

import App.Fossa.Config.ConfigFile (
  ConfigFile (configApiKey, configProject, configRevision, configServer),
  ConfigProject (configProjID),
  ConfigRevision (configBranch, configCommit),
  mergeFileCmdMetadata,
 )
import App.Fossa.Config.EnvironmentVars (EnvVars (..))
import App.Fossa.ProjectInference (
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
  ProjectMetadata (ProjectMetadata),
  ProjectRevision,
  ReleaseGroupMetadata (ReleaseGroupMetadata),
 )
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  errCtx,
  fatalText,
  fromMaybeText,
  (<||>),
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Timeout (Duration (Minutes))
import Data.Bifunctor (Bifunctor (first))
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import Discovery.Filters (targetFilterParser)
import Effect.Exec (Exec)
import Effect.ReadFS (ReadFS, doesDirExist, doesFileExist)
import Fossa.API.Types (ApiKey (ApiKey), ApiOpts (ApiOpts))
import Options.Applicative (
  Parser,
  argument,
  help,
  long,
  metavar,
  optional,
  short,
  str,
  strOption,
  switch,
  value,
  (<|>),
 )
import Path (Abs, Dir, File, Path, Rel, parseRelDir)
import Path.IO (resolveDir', resolveFile')
import Text.Megaparsec (errorBundlePretty, runParser)
import Text.URI (URI, mkURI)
import Types (TargetFilter)

data ScanDestination
  = -- | upload to fossa with provided api key and base url
    UploadScan ApiOpts ProjectMetadata
  | OutputStdout
  deriving (Eq, Ord, Show)

data CacheAction
  = ReadOnly
  | WriteOnly
  deriving (Eq, Ord, Show)

defaultTimeoutDuration :: Duration
defaultTimeoutDuration = Minutes 60

metadataOpts :: Parser ProjectMetadata
metadataOpts =
  ProjectMetadata
    <$> optional (strOption (long "title" <> short 't' <> help "the title of the FOSSA project. (default: the project name)"))
    <*> optional (strOption (long "project-url" <> short 'P' <> help "this repository's home page"))
    <*> optional (strOption (long "jira-project-key" <> short 'j' <> help "this repository's JIRA project key"))
    <*> optional (strOption (long "link" <> short 'L' <> help "a link to attach to the current build"))
    <*> optional (strOption (long "team" <> short 'T' <> help "this repository's team inside your organization"))
    <*> optional (strOption (long "policy" <> help "the policy to assign to this project in FOSSA"))
    <*> optional releaseGroupMetadataOpts

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
  pure $ ApiKey textkey

collectApiOpts :: (Has Diagnostics sig m) => Maybe ConfigFile -> EnvVars -> CommonOpts -> m ApiOpts
collectApiOpts maybeconfig envvars globals = do
  apikey <- validateApiKey maybeconfig envvars globals
  let configUri = maybeconfig >>= configServer >>= mkURI
      baseuri = optBaseUrl globals <|> configUri
  pure $ ApiOpts baseuri apikey

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
      inferred <- inferProjectFromVCS basedir <||> inferProjectCached basedir <||> inferProjectDefault basedir
      pure $ mergeOverride override inferred
    WriteOnly -> do
      inferred <- inferProjectFromVCS basedir <||> inferProjectDefault basedir
      let revision = mergeOverride override inferred
      saveRevision revision
      pure revision

collectRevisionData' ::
  ( Has Diagnostics sig m
  , Has Exec sig m
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

collectAPIMetadata :: Maybe ConfigFile -> ProjectMetadata -> ProjectMetadata
collectAPIMetadata cfgfile cliMeta = maybe cliMeta (mergeFileCmdMetadata cliMeta) cfgfile

data CommonOpts = CommonOpts
  { optDebug :: Bool
  , optBaseUrl :: Maybe URI
  , optProjectName :: Maybe Text
  , optProjectRevision :: Maybe Text
  , optAPIKey :: Maybe Text
  , optConfig :: Maybe FilePath
  }
  deriving (Eq, Ord, Show)

commonOpts :: Parser CommonOpts
commonOpts =
  CommonOpts
    <$> switch (long "debug" <> help "Enable debug logging, and write detailed debug information to `fossa.debug.json`")
    <*> optional (uriOption (long "endpoint" <> short 'e' <> metavar "URL" <> help "The FOSSA API server base URL (default: https://app.fossa.com)"))
    <*> optional (strOption (long "project" <> short 'p' <> help "this repository's URL or VCS endpoint (default: VCS remote 'origin')"))
    <*> optional (strOption (long "revision" <> short 'r' <> help "this repository's current revision hash (default: VCS hash HEAD)"))
    <*> optional (strOption (long "fossa-api-key" <> help "the FOSSA API server authentication key (default: FOSSA_API_KEY from env)"))
    <*> optional (strOption (long "config" <> short 'c' <> help "Path to configuration file including filename (default: .fossa.yml)"))
