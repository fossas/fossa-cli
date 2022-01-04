{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Analyze (
  AnalyzeCliOpts,
  AnalyzeConfig (..),
  BinaryDiscovery (..),
  ExperimentalAnalyzeConfig (..),
  IATAssertion (..),
  IncludeAll (..),
  JsonOutput (..),
  ScanDestination (..),
  UnpackArchives (..),
  VSIAnalysis (..),
  VSIModeOptions (..),
  mkSubCommand,
) where

import App.Fossa.VSI.Types qualified as VSI
import App.Fossa.Config.Common (
  CacheAction (WriteOnly),
  GlobalOpts (..),
  ScanDestination (..),
  baseDirArg,
  collectBaseDir,
  collectRevisionData,
  filterOpt,
  globalOpts,
  metadataOpts,
  pathOpt,
  targetOpt,
  validateApiKey,
  validateDir,
 )
import App.Fossa.Config.ConfigFile (
  ConfigFile (..),
  ConfigPaths (..),
  ConfigTargets (..),
  ExperimentalConfigs (..),
  ExperimentalGradleConfigs (..),
  mergeFileCmdMetadata,
  resolveConfigFile,
 )
import App.Fossa.EnvironmentVars (EnvVars)
import App.Fossa.Subcommand (EffStack, GetSeverity (getSeverity), SubCommand (SubCommand))
import App.Types (
  BaseDir,
  MonorepoAnalysisOpts (MonorepoAnalysisOpts),
  OverrideProject (OverrideProject),
  ProjectMetadata,
  ProjectRevision,
 )
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  Validator,
  runValidation,
  validationBoundary,
 )
import Control.Effect.Lift (Lift, sendIO)
import Data.Flag (Flag, flagOpt)
import Data.Monoid.Extra (isMempty)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (
  ToString (toString),
  ToText (toText),
 )
import Data.Text (Text)
import Discovery.Filters (AllFilters (AllFilters), comboExclude, comboInclude)
import Effect.Exec (
  Exec,
 )
import Effect.Logger (Logger, Severity (SevDebug, SevInfo), logWarn)
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts (ApiOpts))
import Options.Applicative (
  Alternative (many),
  InfoMod,
  Parser,
  eitherReader,
  help,
  hidden,
  long,
  metavar,
  option,
  optional,
  progDesc,
  short,
  strOption,
  switch,
 )
import Path (Abs, Dir, Path, Rel)
import Path.IO (getCurrentDir)
import Types (TargetFilter)

-- CLI flags, for use with 'Data.Flag'
data BinaryDiscovery = BinaryDiscovery

data IncludeAll = IncludeAll

data JsonOutput = JsonOutput

data UnpackArchives = UnpackArchives

data VSIAnalysis = VSIAnalysis

newtype IATAssertion = IATAssertion {unIATAssertion :: Maybe (Path Abs Dir)} deriving (Eq, Ord, Show)

data VSIModeOptions = VSIModeOptions
  { vsiAnalysisEnabled :: Flag VSIAnalysis
  , vsiSkipSet :: VSI.SkipResolution
  , iatAssertion :: IATAssertion
  , binaryDiscoveryEnabled :: Flag BinaryDiscovery
  }
  deriving (Eq, Ord, Show)

data AnalyzeCliOpts = AnalyzeCliOpts
  { globals :: GlobalOpts
  , analyzeOutput :: Bool
  , analyzeUnpackArchives :: Flag UnpackArchives
  , analyzeJsonOutput :: Flag JsonOutput
  , analyzeIncludeAllDeps :: Flag IncludeAll
  , analyzeBranch :: Maybe Text
  , analyzeMetadata :: ProjectMetadata
  , analyzeBuildTargetFilters :: [TargetFilter]
  , analyzeOnlyTargets :: [TargetFilter]
  , analyzeExcludeTargets :: [TargetFilter]
  , analyzeOnlyPaths :: [Path Rel Dir]
  , analyzeExcludePaths :: [Path Rel Dir]
  , analyzeVSIMode :: Flag VSIAnalysis
  , analyzeBinaryDiscoveryMode :: Flag BinaryDiscovery
  , analyzeAssertMode :: Maybe (FilePath)
  , analyzeSkipVSIGraphResolution :: [VSI.Locator]
  , monorepoAnalysisOpts :: MonorepoAnalysisOpts
  , analyzeBaseDir :: FilePath
  }
  deriving (Eq, Ord, Show)

instance GetSeverity AnalyzeCliOpts where
  getSeverity AnalyzeCliOpts{globals = GlobalOpts{optDebug}} = if optDebug then SevDebug else SevInfo

data AnalyzeConfig = AnalyzeConfig
  { baseDir :: BaseDir
  , severity :: Severity
  , scanDestination :: ScanDestination
  , projectRevision :: ProjectRevision
  , vsiOptions :: VSIModeOptions
  , filterSet :: AllFilters
  , experimental :: ExperimentalAnalyzeConfig
  , unpackArchives :: Flag UnpackArchives
  , jsonOutput :: Flag JsonOutput
  , includeAllDeps :: Flag IncludeAll
  }
  deriving (Eq, Ord, Show)

newtype ExperimentalAnalyzeConfig = ExperimentalAnalyzeConfig
  { allowedGradleConfigs :: Maybe (Set Text)
  }
  deriving (Eq, Ord, Show)

mkSubCommand :: (AnalyzeConfig -> EffStack ()) -> SubCommand AnalyzeCliOpts AnalyzeConfig
mkSubCommand = SubCommand "analyze" analyzeInfo cliParser loadConfig mergeOpts

analyzeInfo :: InfoMod a
analyzeInfo = progDesc "Scan for projects and their dependencies"

cliParser :: Parser AnalyzeCliOpts
cliParser =
  AnalyzeCliOpts
    <$> globalOpts
    <*> switch (long "output" <> short 'o' <> help "Output results to stdout instead of uploading to fossa")
    <*> flagOpt UnpackArchives (long "unpack-archives" <> help "Recursively unpack and analyze discovered archives")
    <*> flagOpt JsonOutput (long "json" <> help "Output project metadata as json to the console. Useful for communicating with the FOSSA API")
    <*> flagOpt IncludeAll (long "include-unused-deps" <> help "Include all deps found, instead of filtering non-production deps.  Ignored by VSI.")
    <*> optional (strOption (long "branch" <> short 'b' <> help "this repository's current branch (default: current VCS branch)"))
    <*> metadataOpts
    <*> many filterOpt
    <*> many (option (eitherReader targetOpt) (long "only-target" <> help "Only scan these targets. See targets.only in the fossa.yml spec." <> metavar "PATH"))
    <*> many (option (eitherReader targetOpt) (long "exclude-target" <> help "Exclude these targets from scanning. See targets.exclude in the fossa.yml spec." <> metavar "PATH"))
    <*> many (option (eitherReader pathOpt) (long "only-path" <> help "Only scan these paths. See paths.only in the fossa.yml spec." <> metavar "PATH"))
    <*> many (option (eitherReader pathOpt) (long "exclude-path" <> help "Exclude these paths from scanning. See paths.exclude in the fossa.yml spec." <> metavar "PATH"))
    <*> flagOpt VSIAnalysis (long "enable-vsi" <> hidden)
    <*> flagOpt BinaryDiscovery (long "experimental-enable-binary-discovery" <> hidden)
    <*> optional (strOption (long "experimental-link-project-binary" <> hidden))
    <*> many skipVSIGraphResolutionOpt
    <*> monorepoOpts
    <*> baseDirArg

skipVSIGraphResolutionOpt :: Parser VSI.Locator
skipVSIGraphResolutionOpt = (option (eitherReader parseLocator) (long "experimental-skip-vsi-graph" <> hidden))
  where
    parseLocator :: String -> Either String VSI.Locator
    parseLocator s = case VSI.parseLocator (toText s) of
      Left err -> Left $ toString (toText err)
      Right loc -> pure loc

monorepoOpts :: Parser MonorepoAnalysisOpts
monorepoOpts =
  MonorepoAnalysisOpts
    <$> optional (strOption (long "experimental-enable-monorepo" <> metavar "MODE" <> help "scan the project in the experimental monorepo mode. Supported modes: aosp"))

loadConfig ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  ) =>
  AnalyzeCliOpts ->
  m (Maybe ConfigFile)
loadConfig AnalyzeCliOpts{globals = GlobalOpts{optConfig}} = do
  -- FIXME: We eventually want to use the basedir to inform the config file root
  configRelBase <- sendIO getCurrentDir
  resolveConfigFile configRelBase optConfig

mergeOpts ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  AnalyzeCliOpts ->
  m AnalyzeConfig
mergeOpts maybeConfig envvars cliOpts@AnalyzeCliOpts{..} = do
  basedir <- collectBaseDir analyzeBaseDir
  let logSeverity = if optDebug globals then SevDebug else SevInfo
  scanDestination <- collectScanDestination maybeConfig envvars cliOpts
  revisionData <-
    collectRevisionData basedir maybeConfig WriteOnly $
      OverrideProject (optProjectName globals) (optProjectRevision globals) (analyzeBranch)
  modeOpts <- collectModeOptions cliOpts
  filters <- collectFilters maybeConfig cliOpts
  let experimentalCfgs = collectExperimental maybeConfig

  runValidation $
    AnalyzeConfig
      <$> basedir
      <*> pure logSeverity
      <*> scanDestination
      <*> revisionData
      <*> modeOpts
      <*> filters
      <*> pure experimentalCfgs
      <*> pure analyzeUnpackArchives
      <*> pure analyzeJsonOutput
      <*> pure analyzeIncludeAllDeps

collectFilters ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  Maybe ConfigFile ->
  AnalyzeCliOpts ->
  m (Validator AllFilters)
collectFilters maybeConfig cliOpts = validationBoundary $ do
  let cliFilters = collectCLIFilters cliOpts
      cfgFileFilters = maybe mempty collectConfigFileFilters maybeConfig
  case (isMempty cliFilters, isMempty cfgFileFilters) of
    (True, True) -> pure mempty
    (False, True) -> pure cliFilters
    (True, False) -> pure cfgFileFilters
    (False, False) ->
      cliFilters
        <$ logWarn "Overriding config file filters with command-line filters"

collectConfigFileFilters :: ConfigFile -> AllFilters
collectConfigFileFilters configFile = do
  let pullFromFile :: (a -> [b]) -> (ConfigFile -> Maybe a) -> [b]
      pullFromFile field section = maybe [] field (section configFile)
      onlyT = pullFromFile targetsOnly configTargets
      onlyP = pullFromFile pathsOnly configPaths
      excludeT = pullFromFile targetsExclude configTargets
      excludeP = pullFromFile pathsExclude configPaths

  AllFilters (comboInclude onlyT onlyP) (comboExclude excludeT excludeP)

collectCLIFilters :: AnalyzeCliOpts -> AllFilters
collectCLIFilters AnalyzeCliOpts{..} =
  AllFilters
    (comboInclude analyzeOnlyTargets analyzeOnlyPaths)
    (comboExclude analyzeExcludeTargets analyzeExcludePaths)

collectExperimental :: Maybe ConfigFile -> ExperimentalAnalyzeConfig
collectExperimental maybeCfg =
  ExperimentalAnalyzeConfig $
    fmap
      gradleConfigsOnly
      (maybeCfg >>= configExperimental >>= gradle)

collectScanDestination ::
  ( Has Diagnostics sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  AnalyzeCliOpts ->
  m (Validator ScanDestination)
collectScanDestination maybeCfgFile envvars AnalyzeCliOpts{..} =
  validationBoundary $
    if analyzeOutput
      then pure OutputStdout
      else do
        apiKey <- validateApiKey maybeCfgFile envvars globals
        let baseuri = optBaseUrl globals
            apiOpts = ApiOpts baseuri apiKey
            metaMerged = maybe analyzeMetadata (mergeFileCmdMetadata analyzeMetadata) (maybeCfgFile)
        pure $ UploadScan apiOpts metaMerged

collectModeOptions ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  AnalyzeCliOpts ->
  m (Validator VSIModeOptions)
collectModeOptions AnalyzeCliOpts{..} = validationBoundary $ do
  assertionDir <- traverse validateDir analyzeAssertMode
  pure
    VSIModeOptions
      { vsiAnalysisEnabled = analyzeVSIMode
      , vsiSkipSet = VSI.SkipResolution $ Set.fromList analyzeSkipVSIGraphResolution
      , iatAssertion = IATAssertion assertionDir
      , binaryDiscoveryEnabled = analyzeBinaryDiscoveryMode
      }
