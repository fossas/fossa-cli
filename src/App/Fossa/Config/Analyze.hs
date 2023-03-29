{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Analyze (
  AnalyzeCliOpts (..),
  AnalyzeConfig (..),
  BinaryDiscovery (..),
  ExperimentalAnalyzeConfig (..),
  ForceVendoredDependencyRescans (..),
  IATAssertion (..),
  DynamicLinkInspect (..),
  IncludeAll (..),
  JsonOutput (..),
  MonorepoAnalyzeConfig (..),
  NoDiscoveryExclusion (..),
  ScanDestination (..),
  StandardAnalyzeConfig (..),
  UnpackArchives (..),
  VendoredDependencyOptions (..),
  VSIAnalysis (..),
  VSIModeOptions (..),
  GoDynamicTactic (..),
  mkSubCommand,
  loadConfig,
  cliParser,
  mergeOpts,
) where

import App.Fossa.Config.Common (
  CacheAction (WriteOnly),
  CommonOpts (..),
  ScanDestination (..),
  baseDirArg,
  collectAPIMetadata,
  collectApiOpts,
  collectBaseDir,
  collectConfigFileFilters,
  collectRevisionData',
  commonOpts,
  metadataOpts,
  pathOpt,
  targetOpt,
  validateDir,
  validateExists,
 )
import App.Fossa.Config.ConfigFile (
  ConfigFile (..),
  ConfigTelemetryScope (NoTelemetry),
  ExperimentalConfigs (..),
  ExperimentalGradleConfigs (..),
  VendoredDependencyConfigs (..),
  mergeFileCmdMetadata,
  resolveConfigFile,
 )
import App.Fossa.Config.EnvironmentVars (EnvVars (..))
import App.Fossa.Subcommand (EffStack, GetCommonOpts (getCommonOpts), GetSeverity (getSeverity), SubCommand (SubCommand))
import App.Fossa.VSI.Types qualified as VSI
import App.Types (
  BaseDir,
  MonorepoAnalysisOpts (MonorepoAnalysisOpts, monorepoAnalysisType),
  OverrideDynamicAnalysisBinary (..),
  OverrideProject (OverrideProject),
  ProjectMetadata (projectLabel),
  ProjectRevision,
 )
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  fatalText,
 )
import Control.Effect.Lift (Lift)
import Control.Monad (when)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Flag (Flag, flagOpt, fromFlag)
import Data.Maybe (isJust)
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
import Effect.Logger (Logger, Severity (SevDebug, SevInfo), logWarn, vsep)
import Effect.ReadFS (ReadFS, getCurrentDir, resolveDir)
import Fossa.API.Types (ApiOpts)
import GHC.Generics (Generic)
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
  (<|>),
 )
import Path (Abs, Dir, Path, Rel)
import Path.Extra (SomePath)
import System.Info qualified as SysInfo
import Types (ArchiveUploadType (..), LicenseScanPathFilters (..), TargetFilter)

-- CLI flags, for use with 'Data.Flag'
data DeprecatedAllowNativeLicenseScan = DeprecatedAllowNativeLicenseScan deriving (Generic)
data ForceVendoredDependencyRescans = ForceVendoredDependencyRescans deriving (Generic)

data BinaryDiscovery = BinaryDiscovery deriving (Generic)
data IncludeAll = IncludeAll deriving (Generic)
data JsonOutput = JsonOutput deriving (Generic)
data NoDiscoveryExclusion = NoDiscoveryExclusion deriving (Generic)
data UnpackArchives = UnpackArchives deriving (Generic)
data VSIAnalysis = VSIAnalysis deriving (Generic)

newtype IATAssertion = IATAssertion {unIATAssertion :: Maybe (Path Abs Dir)} deriving (Eq, Ord, Show, Generic)
newtype DynamicLinkInspect = DynamicLinkInspect {unDynamicLinkInspect :: Maybe SomePath} deriving (Eq, Ord, Show, Generic)

instance ToJSON BinaryDiscovery where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON IncludeAll where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON JsonOutput where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON NoDiscoveryExclusion where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON UnpackArchives where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON VSIAnalysis where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON IATAssertion where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON DynamicLinkInspect where
  toEncoding = genericToEncoding defaultOptions

data VSIModeOptions = VSIModeOptions
  { vsiAnalysisEnabled :: Flag VSIAnalysis
  , vsiSkipSet :: VSI.SkipResolution
  , iatAssertion :: IATAssertion
  , dynamicLinkingTarget :: DynamicLinkInspect
  , binaryDiscoveryEnabled :: Flag BinaryDiscovery
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON VSIModeOptions where
  toEncoding = genericToEncoding defaultOptions

data VendoredDependencyOptions = VendoredDependencyOptions
  { forceRescans :: Bool
  , licenseScanMethod :: Maybe ArchiveUploadType
  , licenseScanPathFilters :: Maybe LicenseScanPathFilters
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON VendoredDependencyOptions where
  toEncoding = genericToEncoding defaultOptions

data AnalyzeCliOpts = AnalyzeCliOpts
  { commons :: CommonOpts
  , analyzeOutput :: Bool
  , analyzeUnpackArchives :: Flag UnpackArchives
  , analyzeJsonOutput :: Flag JsonOutput
  , analyzeIncludeAllDeps :: Flag IncludeAll
  , analyzeNoDiscoveryExclusion :: Flag NoDiscoveryExclusion
  , analyzeDeprecatedAllowNativeLicenseScan :: Flag DeprecatedAllowNativeLicenseScan
  , analyzeForceVendoredDependencyMode :: Maybe ArchiveUploadType
  , analyzeForceVendoredDependencyRescans :: Flag ForceVendoredDependencyRescans
  , analyzeBranch :: Maybe Text
  , analyzeMetadata :: ProjectMetadata
  , analyzeOnlyTargets :: [TargetFilter]
  , analyzeExcludeTargets :: [TargetFilter]
  , analyzeOnlyPaths :: [Path Rel Dir]
  , analyzeExcludePaths :: [Path Rel Dir]
  , analyzeVSIMode :: Flag VSIAnalysis
  , analyzeBinaryDiscoveryMode :: Flag BinaryDiscovery
  , analyzeAssertMode :: Maybe (FilePath)
  , analyzeDynamicLinkTarget :: Maybe (FilePath)
  , analyzeSkipVSIGraphResolution :: [VSI.Locator]
  , monorepoAnalysisOpts :: MonorepoAnalysisOpts
  , analyzeBaseDir :: FilePath
  , analyzeDynamicGoAnalysisType :: GoDynamicTactic
  }
  deriving (Eq, Ord, Show)

instance GetCommonOpts AnalyzeCliOpts where
  getCommonOpts AnalyzeCliOpts{analyzeOutput, commons} =
    if analyzeOutput
      then Just commons{optTelemetry = Just NoTelemetry} -- When `--output` is used don't emit no telemetry.
      else Just commons

instance GetSeverity AnalyzeCliOpts where
  getSeverity AnalyzeCliOpts{commons = CommonOpts{optDebug}} = if optDebug then SevDebug else SevInfo

data AnalyzeConfig
  = Monorepo MonorepoAnalyzeConfig
  | Standard StandardAnalyzeConfig
  deriving (Show, Generic)

instance ToJSON AnalyzeConfig where
  toEncoding = genericToEncoding defaultOptions

data MonorepoAnalyzeConfig = MonorepoAnalyzeConfig
  { monorepoAnalyzeOpts :: MonorepoAnalysisOpts
  , monorepoApiOpts :: ApiOpts
  , monorepoBasedir :: BaseDir
  , monorepoFilters :: AllFilters
  , monorepoMetadata :: ProjectMetadata
  , monorepoRevision :: ProjectRevision
  , monorepoSeverity :: Severity
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON MonorepoAnalyzeConfig where
  toEncoding = genericToEncoding defaultOptions

data StandardAnalyzeConfig = StandardAnalyzeConfig
  { baseDir :: BaseDir
  , severity :: Severity
  , scanDestination :: ScanDestination
  , projectRevision :: ProjectRevision
  , vsiOptions :: VSIModeOptions
  , filterSet :: AllFilters
  , experimental :: ExperimentalAnalyzeConfig
  , vendoredDeps :: VendoredDependencyOptions
  , unpackArchives :: Flag UnpackArchives
  , jsonOutput :: Flag JsonOutput
  , includeAllDeps :: Flag IncludeAll
  , noDiscoveryExclusion :: Flag NoDiscoveryExclusion
  , overrideDynamicAnalysis :: OverrideDynamicAnalysisBinary
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON StandardAnalyzeConfig where
  toEncoding = genericToEncoding defaultOptions

data ExperimentalAnalyzeConfig = ExperimentalAnalyzeConfig
  { allowedGradleConfigs :: Maybe (Set Text)
  , useV3GoResolver :: GoDynamicTactic
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ExperimentalAnalyzeConfig where
  toEncoding = genericToEncoding defaultOptions

mkSubCommand :: (AnalyzeConfig -> EffStack ()) -> SubCommand AnalyzeCliOpts AnalyzeConfig
mkSubCommand = SubCommand "analyze" analyzeInfo cliParser loadConfig mergeOpts

analyzeInfo :: InfoMod a
analyzeInfo = progDesc "Scan for projects and their dependencies"

cliParser :: Parser AnalyzeCliOpts
cliParser =
  AnalyzeCliOpts
    <$> commonOpts
    <*> switch (long "output" <> short 'o' <> help "Output results to stdout instead of uploading to fossa")
    <*> flagOpt UnpackArchives (long "unpack-archives" <> help "Recursively unpack and analyze discovered archives")
    <*> flagOpt JsonOutput (long "json" <> help "Output project metadata as json to the console. Useful for communicating with the FOSSA API")
    <*> flagOpt IncludeAll (long "include-unused-deps" <> help "Include all deps found, instead of filtering non-production deps.  Ignored by VSI.")
    <*> flagOpt NoDiscoveryExclusion (long "debug-no-discovery-exclusion" <> help "Ignore filters during discovery phase.  This is for debugging only and may be removed without warning." <> hidden)
    -- AllowNativeLicenseScan is no longer used. We started emitting a warning if it was used in https://github.com/fossas/fossa-cli/pull/1113
    <*> flagOpt DeprecatedAllowNativeLicenseScan (long "experimental-native-license-scan" <> hidden)
    <*> optional vendoredDependencyModeOpt
    <*> flagOpt ForceVendoredDependencyRescans (long "force-vendored-dependency-rescans" <> help "Force vendored dependencies to be rescanned even if the revision has been previously analyzed by FOSSA. This currently only works for CLI-side license scans.")
    <*> optional (strOption (long "branch" <> short 'b' <> help "this repository's current branch (default: current VCS branch)"))
    <*> metadataOpts
    <*> many (option (eitherReader targetOpt) (long "only-target" <> help "Only scan these targets. See targets.only in the fossa.yml spec." <> metavar "PATH"))
    <*> many (option (eitherReader targetOpt) (long "exclude-target" <> help "Exclude these targets from scanning. See targets.exclude in the fossa.yml spec." <> metavar "PATH"))
    <*> many (option (eitherReader pathOpt) (long "only-path" <> help "Only scan these paths. See paths.only in the fossa.yml spec." <> metavar "PATH"))
    <*> many (option (eitherReader pathOpt) (long "exclude-path" <> help "Exclude these paths from scanning. See paths.exclude in the fossa.yml spec." <> metavar "PATH"))
    <*> vsiEnableOpt
    <*> flagOpt BinaryDiscovery (long "experimental-enable-binary-discovery" <> help "Reports binary files as unlicensed dependencies")
    <*> optional (strOption (long "experimental-link-project-binary" <> metavar "DIR" <> help "Links output binary files to this project in FOSSA"))
    <*> optional dynamicLinkInspectOpt
    <*> many skipVSIGraphResolutionOpt
    <*> monorepoOpts
    <*> baseDirArg
    <*> experimentalUseV3GoResolver

data GoDynamicTactic
  = GoModulesBasedTactic
  | GoPackagesBasedTactic
  deriving (Eq, Ord, Show, Generic)

instance ToJSON GoDynamicTactic where
  toEncoding = genericToEncoding defaultOptions

experimentalUseV3GoResolver :: Parser GoDynamicTactic
experimentalUseV3GoResolver =
  fmap
    ( \case
        True -> GoPackagesBasedTactic
        False -> GoModulesBasedTactic
    )
    . switch
    $ long "experimental-use-v3-go-resolver"
      <> help "For Go: generate a graph of module deps based on package deps. This will be the default in the future."

vendoredDependencyModeOpt :: Parser ArchiveUploadType
vendoredDependencyModeOpt = option (eitherReader parseType) (long "force-vendored-dependency-scan-method" <> metavar "METHOD" <> help "Force the vendored dependency scan method. The options are 'CLILicenseScan' or 'ArchiveUpload'. 'CLILicenseScan' is usually the default unless your organization has overridden this.")
  where
    parseType :: String -> Either String ArchiveUploadType
    parseType = \case
      "ArchiveUpload" -> Right ArchiveUpload
      "CLILicenseScan" -> Right CLILicenseScan
      val -> Left ("must be either 'CLILicenseScan' or 'ArchiveUpload'. Found " <> val)

dynamicLinkInspectOpt :: Parser FilePath
dynamicLinkInspectOpt = visible <|> legacy
  where
    visible = strOption (long "detect-dynamic" <> metavar "BINARY" <> help "Analyzes dynamically linked libraries in the target binary and reports them as dependencies")
    legacy = strOption (long "experimental-analyze-dynamic-deps" <> hidden)

vsiEnableOpt :: Parser (Flag VSIAnalysis)
vsiEnableOpt = visible <|> legacyExperimental <|> legacy
  where
    visible = flagOpt VSIAnalysis (long "detect-vendored" <> help "Analyzes project files on disk to detect vendored open source libraries")
    legacyExperimental = flagOpt VSIAnalysis (long "experimental-enable-vsi" <> hidden)
    legacy = flagOpt VSIAnalysis (long "enable-vsi" <> hidden)

skipVSIGraphResolutionOpt :: Parser VSI.Locator
skipVSIGraphResolutionOpt = (option (eitherReader parseLocator) details)
  where
    details =
      mconcat
        [ long "experimental-skip-vsi-graph"
        , metavar "LOCATOR"
        , help "Skip resolving the dependencies of the given project in FOSSA"
        ]
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
loadConfig AnalyzeCliOpts{analyzeBaseDir, commons = CommonOpts{optConfig}} = do
  cwd <- getCurrentDir
  configBaseDir <- resolveDir cwd (toText analyzeBaseDir)
  resolveConfigFile configBaseDir optConfig

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
mergeOpts cfg env cliOpts = do
  let experimentalNativeLicenseScanFlagUsed = fromFlag DeprecatedAllowNativeLicenseScan $ analyzeDeprecatedAllowNativeLicenseScan cliOpts
  when experimentalNativeLicenseScanFlagUsed $ do
    logWarn $
      vsep
        [ "DEPRECATION NOTICE"
        , "========================"
        , "The --experimental-native-license-scan flag is deprecated."
        , ""
        , "The functionality enabled by the flag, CLI-license-scans, is now the default method for scanning vendored-dependencies."
        , ""
        , "In the future, usage of the --experimental-native-license-scan flag may result in fatal error."
        ]

  if isJust $ monorepoAnalysisType $ monorepoAnalysisOpts cliOpts
    then Monorepo <$> mergeMonorepoOpts cfg env cliOpts
    else Standard <$> mergeStandardOpts cfg env cliOpts

mergeMonorepoOpts ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  AnalyzeCliOpts ->
  m MonorepoAnalyzeConfig
mergeMonorepoOpts cfgfile envvars cliOpts@AnalyzeCliOpts{..} = do
  let monoOpts = monorepoAnalysisOpts
      metadata = collectAPIMetadata cfgfile analyzeMetadata
      severity = getSeverity cliOpts
      apiopts = collectApiOpts cfgfile envvars commons
      basedir = collectBaseDir analyzeBaseDir
      filters = collectFilters cfgfile cliOpts
      revision =
        collectRevisionData' basedir cfgfile WriteOnly $
          OverrideProject (optProjectName commons) (optProjectRevision commons) (analyzeBranch)
      failureOnWindows = fatalOnWindows "Monorepo analysis is not supported on windows"
      failureOnOutput = when analyzeOutput $ fatalText "Monorepo analysis does not support the `--output` flag"

  MonorepoAnalyzeConfig
    monoOpts
    <$> apiopts
    <*> basedir
    <*> filters
    <*> pure metadata
    <*> revision
    <*> pure severity
    -- Add phantom failures here (no data to return)
    <* failureOnWindows
    <* failureOnOutput

windowsOsName :: String
windowsOsName = "mingw32"

fatalOnWindows :: Has Diagnostics sig m => Text -> m ()
fatalOnWindows msg = when (SysInfo.os == windowsOsName) $ fatalText msg

mergeStandardOpts ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  AnalyzeCliOpts ->
  m StandardAnalyzeConfig
mergeStandardOpts maybeConfig envvars cliOpts@AnalyzeCliOpts{..} = do
  let basedir = collectBaseDir analyzeBaseDir
      logSeverity = getSeverity cliOpts
      scanDestination = collectScanDestination maybeConfig envvars cliOpts
      revisionData =
        collectRevisionData' basedir maybeConfig WriteOnly $
          OverrideProject (optProjectName commons) (optProjectRevision commons) (analyzeBranch)
      modeOpts = collectModeOptions cliOpts
      filters = collectFilters maybeConfig cliOpts
      experimentalCfgs = collectExperimental maybeConfig cliOpts
      vendoredDepsOptions = collectVendoredDeps maybeConfig cliOpts
      dynamicAnalysisOverrides = OverrideDynamicAnalysisBinary $ envCmdOverrides envvars

  StandardAnalyzeConfig
    <$> basedir
    <*> pure logSeverity
    <*> scanDestination
    <*> revisionData
    <*> modeOpts
    <*> filters
    <*> pure experimentalCfgs
    <*> vendoredDepsOptions
    <*> pure analyzeUnpackArchives
    <*> pure analyzeJsonOutput
    <*> pure analyzeIncludeAllDeps
    <*> pure analyzeNoDiscoveryExclusion
    <*> pure dynamicAnalysisOverrides

collectFilters ::
  ( Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  Maybe ConfigFile ->
  AnalyzeCliOpts ->
  m AllFilters
collectFilters maybeConfig cliOpts = do
  let cliFilters = collectCLIFilters cliOpts
      cfgFileFilters = maybe mempty collectConfigFileFilters maybeConfig
  case (isMempty cliFilters, isMempty cfgFileFilters) of
    (True, True) -> pure mempty
    (False, True) -> pure cliFilters
    (True, False) -> pure cfgFileFilters
    (False, False) ->
      cliFilters
        <$ logWarn "Overriding config file filters with command-line filters"

collectCLIFilters :: AnalyzeCliOpts -> AllFilters
collectCLIFilters AnalyzeCliOpts{..} =
  AllFilters
    (comboInclude analyzeOnlyTargets analyzeOnlyPaths)
    (comboExclude analyzeExcludeTargets analyzeExcludePaths)

collectExperimental :: Maybe ConfigFile -> AnalyzeCliOpts -> ExperimentalAnalyzeConfig
collectExperimental maybeCfg AnalyzeCliOpts{analyzeDynamicGoAnalysisType = goDynamicAnalysisType} =
  ExperimentalAnalyzeConfig
    ( fmap
        gradleConfigsOnly
        (maybeCfg >>= configExperimental >>= gradle)
    )
    goDynamicAnalysisType

collectVendoredDeps ::
  ( Has Diagnostics sig m
  ) =>
  Maybe ConfigFile ->
  AnalyzeCliOpts ->
  m VendoredDependencyOptions
collectVendoredDeps maybeCfg cliOpts = do
  let (forceRescansFromFlags, scanTypeFromFlags) = collectVendoredDepsFromFlags cliOpts
      (forceRescansFromConfig, scanTypeFromConfig, licenseScanPathFiltersFromConfig) = collectVendoredDepsFromConfig maybeCfg
  pure $ VendoredDependencyOptions (forceRescansFromFlags || forceRescansFromConfig) (scanTypeFromFlags <|> scanTypeFromConfig) licenseScanPathFiltersFromConfig

collectVendoredDepsFromFlags ::
  AnalyzeCliOpts ->
  (Bool, Maybe ArchiveUploadType)
collectVendoredDepsFromFlags AnalyzeCliOpts{..} = do
  let forceRescans = fromFlag ForceVendoredDependencyRescans analyzeForceVendoredDependencyRescans
      scanType = analyzeForceVendoredDependencyMode
  (forceRescans, scanType)

collectVendoredDepsFromConfig :: Maybe ConfigFile -> (Bool, Maybe ArchiveUploadType, Maybe LicenseScanPathFilters)
collectVendoredDepsFromConfig maybeCfg =
  let forceRescans = maybe False configForceRescans (maybeCfg >>= configVendoredDependencies)
      defaultScanType = maybeCfg >>= configVendoredDependencies >>= configLicenseScanMethod
      pathFilters = maybeCfg >>= configVendoredDependencies >>= configLicenseScanPathFilters
   in (forceRescans, defaultScanType, pathFilters)

collectScanDestination ::
  ( Has Diagnostics sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  AnalyzeCliOpts ->
  m ScanDestination
collectScanDestination maybeCfgFile envvars AnalyzeCliOpts{..} =
  if analyzeOutput
    then pure OutputStdout
    else do
      apiOpts <- collectApiOpts maybeCfgFile envvars commons
      let metaMerged = maybe analyzeMetadata (mergeFileCmdMetadata analyzeMetadata) (maybeCfgFile)
      when (length (projectLabel metaMerged) > 5) $ fatalText "Projects are only allowed to have 5 associated project labels"
      pure $ UploadScan apiOpts metaMerged

collectModeOptions ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  AnalyzeCliOpts ->
  m VSIModeOptions
collectModeOptions AnalyzeCliOpts{..} = do
  assertionDir <- traverse validateDir analyzeAssertMode
  resolvedDynamicLinkTarget <- traverse validateExists analyzeDynamicLinkTarget
  pure
    VSIModeOptions
      { vsiAnalysisEnabled = analyzeVSIMode
      , vsiSkipSet = VSI.SkipResolution $ Set.fromList analyzeSkipVSIGraphResolution
      , iatAssertion = IATAssertion assertionDir
      , dynamicLinkingTarget = DynamicLinkInspect resolvedDynamicLinkTarget
      , binaryDiscoveryEnabled = analyzeBinaryDiscoveryMode
      }
