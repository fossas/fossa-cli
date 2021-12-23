{-# LANGUAGE RecordWildCards #-}

module App.NewFossa.Config.Common (
  -- * CLI Parsers
  GlobalOpts (..),
  globalOpts,
  releaseGroupMetadataOpts,
  filterOpt,
  pathOpt,
  targetOpt,
  baseDirArg,
  metadataOpts,

  -- * CLI Validators
  validateDir,
  validateApiKey,

  -- * CLI Collectors
  collectBaseDir,
  collectRevisionData,
  collectRevisionOverride,
  collectApiOpts,

  -- * Configuration Types
  ScanDestination (..),
) where

import App.Fossa.ProjectInference (
  inferProjectDefault,
  inferProjectFromVCS,
  mergeOverride,
 )
import App.NewFossa.ConfigFile (
  ConfigFile (configApiKey, configProject, configRevision),
  ConfigProject (configProjID),
  ConfigRevision (configBranch, configCommit),
 )
import App.NewFossa.EnvironmentVars (EnvVars (..))
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
  Validation (Failure, Success),
  Validator,
  fatalText,
  fromMaybeText,
  validationBoundary,
  (<||>),
 )
import Control.Effect.Lift (Lift, sendIO)
import Data.Bifunctor (Bifunctor (first))
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import Discovery.Filters (targetFilterParser)
import Effect.Exec (Exec)
import Effect.ReadFS (ReadFS, doesDirExist)
import Fossa.API.Types (ApiKey (ApiKey), ApiOpts (ApiOpts))
import Options.Applicative (
  Parser,
  argument,
  eitherReader,
  help,
  long,
  metavar,
  option,
  optional,
  short,
  str,
  strOption,
  switch,
  value,
  (<|>),
 )
import Path (Abs, Dir, Path, Rel, parseRelDir)
import Path.IO (resolveDir')
import Text.Megaparsec (errorBundlePretty, runParser)
import Text.URI (URI)
import Types (TargetFilter)

data ScanDestination
  = -- | upload to fossa with provided api key and base url
    UploadScan ApiOpts ProjectMetadata
  | OutputStdout
  deriving (Eq, Ord, Show)

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

filterOpt :: Parser TargetFilter
filterOpt = option (eitherReader parseFilter) (long "filter" <> help "(deprecated) Analysis-Target filters (default: none)" <> metavar "ANALYSIS-TARGET")
  where
    parseFilter :: String -> Either String TargetFilter
    parseFilter = first errorBundlePretty . runParser targetFilterParser "(Command-line arguments)" . toText

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
  m (Validator BaseDir)
collectBaseDir baseDirFp = validationBoundary $ BaseDir <$> validateDir baseDirFp

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

validateApiKey ::
  ( Has Diagnostics sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  GlobalOpts ->
  m ApiKey
validateApiKey maybeConfigFile EnvVars{envApiKey} GlobalOpts{optAPIKey} = do
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

collectApiOpts :: (Has Diagnostics sig m) => Maybe ConfigFile -> EnvVars -> GlobalOpts -> m (Validator ApiOpts)
collectApiOpts maybeconfig envvars globals = validationBoundary $ do
  apikey <- validateApiKey maybeconfig envvars globals
  let baseuri = optBaseUrl globals
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

collectRevisionData ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  Validator BaseDir ->
  Maybe ConfigFile ->
  OverrideProject ->
  m (Validator ProjectRevision)
collectRevisionData (Failure _) _ _ = validationBoundary $ fatalText "Cannot perform revision inference without a valid base directory"
collectRevisionData (Success (BaseDir basedir)) maybeConfig cliOverride = validationBoundary $ do
  let override = collectRevisionOverride maybeConfig cliOverride
  inferred <- inferProjectFromVCS basedir <||> inferProjectDefault basedir
  pure $ mergeOverride override inferred

data GlobalOpts = GlobalOpts
  { optDebug :: Bool
  , optBaseUrl :: Maybe URI
  , optProjectName :: Maybe Text
  , optProjectRevision :: Maybe Text
  , optAPIKey :: Maybe Text
  , optConfig :: Maybe FilePath
  }
  deriving (Eq, Ord, Show)

globalOpts :: Parser GlobalOpts
globalOpts =
  GlobalOpts
    <$> switch (long "debug" <> help "Enable debug logging, and write detailed debug information to `fossa.debug.json`")
    <*> optional (uriOption (long "endpoint" <> short 'e' <> metavar "URL" <> help "The FOSSA API server base URL (default: https://app.fossa.com)"))
    <*> optional (strOption (long "project" <> short 'p' <> help "this repository's URL or VCS endpoint (default: VCS remote 'origin')"))
    <*> optional (strOption (long "revision" <> short 'r' <> help "this repository's current revision hash (default: VCS hash HEAD)"))
    <*> optional (strOption (long "fossa-api-key" <> help "the FOSSA API server authentication key (default: FOSSA_API_KEY from env)"))
    <*> optional (strOption (long "config" <> short 'c' <> help "Path to configuration file including filename (default: .fossa.yml)"))
