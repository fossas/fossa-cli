{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Analyze (
  AnalyzeCliOpts (..),
  BinaryDiscovery (..),
  ExperimentalAnalyzeConfig (..),
  ForceVendoredDependencyRescans (..),
  ForceFirstPartyScans (..),
  ForceNoFirstPartyScans (..),
  IATAssertion (..),
  DynamicLinkInspect (..),
  IncludeAll (..),
  JsonOutput (..),
  NoDiscoveryExclusion (..),
  ScanDestination (..),
  AnalysisTacticTypes (..),
  AnalyzeConfig (..),
  UnpackArchives (..),
  VendoredDependencyOptions (..),
  VSIAnalysis (..),
  VSIModeOptions (..),
  GoDynamicTactic (..),
  StaticOnlyTactics (..),
  mkSubCommand,
  loadConfig,
  cliParser,
  mergeOpts,
  branchHelp,
) where

import App.Fossa.Config.Common (
  CacheAction (WriteOnly),
  CommonOpts (..),
  ScanDestination (..),
  baseDirArg,
  collectApiOpts,
  collectBaseDir,
  collectConfigFileFilters,
  collectConfigMavenScopeFilters,
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
  ConfigGrepEntry (..),
  ConfigTelemetryScope (NoTelemetry),
  ExperimentalConfigs (..),
  ExperimentalGradleConfigs (..),
  OrgWideCustomLicenseConfigPolicy (..),
  VendoredDependencyConfigs (..),
  mergeFileCmdMetadata,
  resolveConfigFile,
 )
import App.Fossa.Config.EnvironmentVars (EnvVars (..))
import App.Fossa.Lernie.Types (GrepEntry (..), GrepOptions (..))
import App.Fossa.Subcommand (EffStack, GetCommonOpts (getCommonOpts), GetSeverity (getSeverity), SubCommand (SubCommand))
import App.Fossa.VSI.Types qualified as VSI
import App.Types (
  BaseDir,
  FirstPartyScansFlag (..),
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
import Data.Monoid.Extra (isMempty)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (
  ToString (toString),
  ToText (toText),
 )
import Data.Text (Text)
import Discovery.Filters (AllFilters (AllFilters), MavenScopeFilters (MavenScopeIncludeFilters), comboExclude, comboInclude)
import Effect.Exec (
  Exec,
 )
import Effect.Logger (Logger, Severity (SevDebug, SevInfo), logWarn, vsep)
import Effect.ReadFS (ReadFS, getCurrentDir, resolveDir)
import GHC.Generics (Generic)
import Options.Applicative (
  Alternative (many),
  InfoMod,
  Parser,
  eitherReader,
  helpDoc,
  hidden,
  long,
  metavar,
  option,
  optional,
  progDescDoc,
  short,
  strOption,
  switch,
  (<|>),
 )
import Path (Abs, Dir, Path, Rel)
import Path.Extra (SomePath)
import Prettyprinter (Doc, annotate, indent)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Green, Red), color)
import Style (applyFossaStyle, boldItalicized, coloredBoldItalicized, formatDoc, formatStringToDoc, stringToHelpDoc)
import Types (ArchiveUploadType (..), LicenseScanPathFilters (..), TargetFilter)

-- CLI flags, for use with 'Data.Flag'
data DeprecatedAllowNativeLicenseScan = DeprecatedAllowNativeLicenseScan deriving (Generic)
data ForceVendoredDependencyRescans = ForceVendoredDependencyRescans deriving (Generic)
data ForceFirstPartyScans = ForceFirstPartyScans deriving (Generic)
data ForceNoFirstPartyScans = ForceNoFirstPartyScans deriving (Generic)
data IgnoreOrgWideCustomLicenseScanConfigs = IgnoreOrgWideCustomLicenseScanConfigs deriving (Generic)
data StaticOnlyTactics = StaticOnlyTactics deriving (Generic)

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
  , analyzeBaseDir :: FilePath
  , analyzeDynamicGoAnalysisType :: GoDynamicTactic
  , analyzePathDependencies :: Bool
  , analyzeForceFirstPartyScans :: Flag ForceFirstPartyScans
  , analyzeForceNoFirstPartyScans :: Flag ForceNoFirstPartyScans
  , analyzeIgnoreOrgWideCustomLicenseScanConfigs :: Flag IgnoreOrgWideCustomLicenseScanConfigs
  , analyzeCustomFossaDepsFile :: Maybe FilePath
  , analyzeStaticOnlyTactics :: Flag StaticOnlyTactics
  }
  deriving (Eq, Ord, Show)

instance GetCommonOpts AnalyzeCliOpts where
  getCommonOpts AnalyzeCliOpts{analyzeOutput, commons} =
    if analyzeOutput
      then Just commons{optTelemetry = Just NoTelemetry} -- When `--output` is used don't emit no telemetry.
      else Just commons

instance GetSeverity AnalyzeCliOpts where
  getSeverity AnalyzeCliOpts{commons = CommonOpts{optDebug}} = if optDebug then SevDebug else SevInfo

data AnalysisTacticTypes = StaticOnly | Any deriving (Eq, Ord, Show, Generic)

instance ToJSON AnalysisTacticTypes where
  toEncoding = genericToEncoding defaultOptions

data AnalyzeConfig = AnalyzeConfig
  { baseDir :: BaseDir
  , severity :: Severity
  , scanDestination :: ScanDestination
  , projectRevision :: ProjectRevision
  , vsiOptions :: VSIModeOptions
  , filterSet :: AllFilters
  , mavenScopeFilterSet :: MavenScopeFilters
  , experimental :: ExperimentalAnalyzeConfig
  , vendoredDeps :: VendoredDependencyOptions
  , unpackArchives :: Flag UnpackArchives
  , jsonOutput :: Flag JsonOutput
  , includeAllDeps :: Flag IncludeAll
  , noDiscoveryExclusion :: Flag NoDiscoveryExclusion
  , overrideDynamicAnalysis :: OverrideDynamicAnalysisBinary
  , firstPartyScansFlag :: FirstPartyScansFlag
  , grepOptions :: GrepOptions
  , customFossaDepsFile :: Maybe FilePath
  , allowedTacticTypes :: AnalysisTacticTypes
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON AnalyzeConfig where
  toEncoding = genericToEncoding defaultOptions

data ExperimentalAnalyzeConfig = ExperimentalAnalyzeConfig
  { allowedGradleConfigs :: Maybe (Set Text)
  , useV3GoResolver :: GoDynamicTactic
  , resolvePathDependencies :: Bool
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ExperimentalAnalyzeConfig where
  toEncoding = genericToEncoding defaultOptions

mkSubCommand :: (AnalyzeConfig -> EffStack ()) -> SubCommand AnalyzeCliOpts AnalyzeConfig
mkSubCommand = SubCommand ("analyze") analyzeInfo cliParser loadConfig mergeOpts

analyzeInfo :: InfoMod a
analyzeInfo =
  progDescDoc $
    Just . formatDoc $
      vsep
        [ "Scan for projects and their dependencies"
        , ""
        , "Examples:"
        , indent 2 $
            vsep
              [ "- " <> coloredBoldItalicized Green "fossa analyze" <> " # Analyze current directory"
              , "- " <> coloredBoldItalicized Green "fossa analyze {path/to/specific/directory}" <> " # Analyze specific directory"
              , "- " <> coloredBoldItalicized Green "fossa analyze --detect-vendored" <> " # Analyze and detect vendored open source libraries in current directory"
              , "- " <> coloredBoldItalicized Green "fossa analyze --debug" <> " # Analyze current directory in debug mode"
              ]
        ]

cliParser :: Parser AnalyzeCliOpts
cliParser =
  AnalyzeCliOpts
    <$> commonOpts
    <*> switch (applyFossaStyle <> long "output" <> short 'o' <> stringToHelpDoc "Output results to stdout instead of uploading to FOSSA")
    <*> flagOpt UnpackArchives (applyFossaStyle <> long "unpack-archives" <> stringToHelpDoc "Recursively unpack and analyze discovered archives")
    <*> flagOpt JsonOutput (applyFossaStyle <> long "json" <> stringToHelpDoc "Output project metadata as JSON to the console. This is useful for communicating with the FOSSA API.")
    <*> flagOpt IncludeAll (applyFossaStyle <> long "include-unused-deps" <> stringToHelpDoc "Include all deps found, instead of filtering non-production deps. Ignored by VSI.")
    <*> flagOpt NoDiscoveryExclusion (applyFossaStyle <> long "debug-no-discovery-exclusion" <> stringToHelpDoc "Ignore filters during discovery phase. This is for debugging only and may be removed without warning." <> hidden)
    -- AllowNativeLicenseScan is no longer used. We started emitting a warning if it was used in https://github.com/fossas/fossa-cli/pull/1113
    <*> flagOpt DeprecatedAllowNativeLicenseScan (applyFossaStyle <> long "experimental-native-license-scan" <> hidden)
    <*> optional vendoredDependencyModeOpt
    <*> flagOpt ForceVendoredDependencyRescans (applyFossaStyle <> long "force-vendored-dependency-rescans" <> stringToHelpDoc "Force vendored dependencies to be rescanned even if the revision has been previously analyzed by FOSSA. This currently only works for CLI-side license scans.")
    <*> optional (strOption (applyFossaStyle <> long "branch" <> short 'b' <> helpDoc branchHelp))
    <*> metadataOpts
    <*> many (option (eitherReader targetOpt) (applyFossaStyle <> long "only-target" <> stringToHelpDoc "Only scan these targets. See `targets.only` in the fossa.yml spec." <> metavar "PATH"))
    <*> many (option (eitherReader targetOpt) (applyFossaStyle <> long "exclude-target" <> stringToHelpDoc "Exclude these targets from scanning. See `targets.exclude` in the fossa.yml spec." <> metavar "PATH"))
    <*> many (option (eitherReader pathOpt) (applyFossaStyle <> long "only-path" <> stringToHelpDoc "Only scan these paths. See `paths.only` in the fossa.yml spec." <> metavar "PATH"))
    <*> many (option (eitherReader pathOpt) (applyFossaStyle <> long "exclude-path" <> stringToHelpDoc "Exclude these paths from scanning. See `paths.exclude` in the fossa.yml spec." <> metavar "PATH"))
    <*> vsiEnableOpt
    <*> flagOpt BinaryDiscovery (applyFossaStyle <> long "experimental-enable-binary-discovery" <> stringToHelpDoc "Reports binary files as unlicensed dependencies")
    <*> optional (strOption (applyFossaStyle <> long "experimental-link-project-binary" <> metavar "DIR" <> stringToHelpDoc "Links output binary files to this project in FOSSA"))
    <*> optional dynamicLinkInspectOpt
    <*> many skipVSIGraphResolutionOpt
    <*> baseDirArg
    <*> experimentalUseV3GoResolver
    <*> experimentalAnalyzePathDependencies
    <*> flagOpt ForceFirstPartyScans (applyFossaStyle <> long "experimental-force-first-party-scans" <> stringToHelpDoc "Force first party scans")
    <*> flagOpt ForceNoFirstPartyScans (applyFossaStyle <> long "experimental-block-first-party-scans" <> stringToHelpDoc "Block first party scans. This can be used to forcibly turn off first-party scans if your organization defaults to first-party scans.")
    <*> flagOpt IgnoreOrgWideCustomLicenseScanConfigs (applyFossaStyle <> long "ignore-org-wide-custom-license-scan-configs" <> stringToHelpDoc "Ignore custom-license scan configurations for your organization. These configurations are defined in the `Integrations` section of the Admin settings in the FOSSA web app")
    <*> optional (strOption (applyFossaStyle <> long "fossa-deps-file" <> helpDoc fossaDepsFileHelp <> metavar "FILEPATH"))
    <*> flagOpt StaticOnlyTactics (applyFossaStyle <> long "static-only-analysis" <> stringToHelpDoc "Only analyze the project using static strategies.")
  where
    fossaDepsFileHelp :: Maybe (Doc AnsiStyle)
    fossaDepsFileHelp =
      Just . formatDoc $
        vsep
          [ "Path to fossa-deps file including filename"
          , boldItalicized "Default:" <> " fossa-deps.{yaml|yml|json}"
          ]
branchHelp :: Maybe (Doc AnsiStyle)
branchHelp =
  Just . formatDoc $
    vsep
      [ "This repository's current branch"
      , boldItalicized "Default: " <> "Current VCS branch"
      ]

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
      <> applyFossaStyle
      <> helpDoc helpMsg
  where
    helpMsg :: Maybe (Doc AnsiStyle)
    helpMsg =
      Just . formatDoc $
        vsep
          [ annotate (color Red) "DEPRECATED: This is now default and will be removed in the future"
          , boldItalicized "For Go: " <> "Generate a graph of module deps based on package deps. This will be the default in the future."
          ]

experimentalAnalyzePathDependencies :: Parser Bool
experimentalAnalyzePathDependencies =
  switch $
    long "experimental-analyze-path-dependencies"
      <> applyFossaStyle
      <> stringToHelpDoc "License scan dependencies sourced from file system, as indicated in manifest files. This will be enabled by default in the future."

vendoredDependencyModeOpt :: Parser ArchiveUploadType
vendoredDependencyModeOpt = option (eitherReader parseType) (applyFossaStyle <> long "force-vendored-dependency-scan-method" <> metavar "METHOD" <> helpDoc vendoredDependencyScanMethodHelp)
  where
    parseType :: String -> Either String ArchiveUploadType
    parseType = \case
      "ArchiveUpload" -> Right ArchiveUpload
      "CLILicenseScan" -> Right CLILicenseScan
      val -> Left ("must be either 'CLILicenseScan' or 'ArchiveUpload'. Found " <> val)

    vendoredDependencyScanMethodHelp :: Maybe (Doc AnsiStyle)
    vendoredDependencyScanMethodHelp =
      Just . formatDoc $
        vsep
          [ "Force the vendored dependency scan method"
          , boldItalicized "Options: " <> coloredBoldItalicized Green "CLILicenseScan" <> boldItalicized "|" <> coloredBoldItalicized Green "ArchiveUpload"
          , boldItalicized "Note: " <> coloredBoldItalicized Green "CLILicenseScan" <> " is usually the default unless your organization has overridden this"
          ]

dynamicLinkInspectOpt :: Parser FilePath
dynamicLinkInspectOpt = visible <|> legacy
  where
    visible = strOption (applyFossaStyle <> long "detect-dynamic" <> metavar "BINARY" <> stringToHelpDoc "Analyzes dynamically linked libraries in the target binary and reports them as dependencies")
    legacy = strOption (applyFossaStyle <> long "experimental-analyze-dynamic-deps" <> hidden)

vsiEnableOpt :: Parser (Flag VSIAnalysis)
vsiEnableOpt = visible <|> legacyExperimental <|> legacy
  where
    visible = flagOpt VSIAnalysis (applyFossaStyle <> long "detect-vendored" <> stringToHelpDoc "Analyzes project files on disk to detect vendored open source libraries")
    legacyExperimental = flagOpt VSIAnalysis (applyFossaStyle <> long "experimental-enable-vsi" <> hidden)
    legacy = flagOpt VSIAnalysis (applyFossaStyle <> long "enable-vsi" <> hidden)

skipVSIGraphResolutionOpt :: Parser VSI.Locator
skipVSIGraphResolutionOpt = (option (eitherReader parseLocator) details)
  where
    details =
      mconcat
        [ long "experimental-skip-vsi-graph"
        , metavar "LOCATOR"
        , stringToHelpDoc "Skip resolving the dependencies of the given project in FOSSA"
        ]
        <> applyFossaStyle
    parseLocator :: String -> Either String VSI.Locator
    parseLocator s = case VSI.parseLocator (toText s) of
      Left err -> Left $ toString (toText err)
      Right loc -> pure loc

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

  mergeStandardOpts cfg env cliOpts

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
  m AnalyzeConfig
mergeStandardOpts maybeConfig envvars cliOpts@AnalyzeCliOpts{..} = do
  let basedir = collectBaseDir analyzeBaseDir
      logSeverity = getSeverity cliOpts
      scanDestination = collectScanDestination maybeConfig envvars cliOpts
      revisionData =
        collectRevisionData' basedir maybeConfig WriteOnly $
          OverrideProject (optProjectName commons) (optProjectRevision commons) (analyzeBranch)
      modeOpts = collectModeOptions cliOpts
      filters = collectFilters maybeConfig cliOpts
      mavenScopeFilters = collectMavenScopeFilters maybeConfig
      experimentalCfgs = collectExperimental maybeConfig cliOpts
      vendoredDepsOptions = collectVendoredDeps maybeConfig cliOpts
      dynamicAnalysisOverrides = OverrideDynamicAnalysisBinary $ envCmdOverrides envvars
      grepOptions = collectGrepOptions maybeConfig cliOpts
      customFossaDepsFile = analyzeCustomFossaDepsFile
      allowedTacticType =
        if fromFlag StaticOnlyTactics analyzeStaticOnlyTactics
          then StaticOnly
          else Any

  firstPartyScansFlag <-
    case (fromFlag ForceFirstPartyScans analyzeForceFirstPartyScans, fromFlag ForceNoFirstPartyScans analyzeForceNoFirstPartyScans) of
      (True, True) -> fatalText "You provided both the --experimental-force-first-party-scans and --experimental-block-first-party-scans flags. Only one of these flags may be used"
      (True, _) -> pure FirstPartyScansOnFromFlag
      (_, True) -> pure FirstPartyScansOffFromFlag
      (False, False) -> pure FirstPartyScansUseDefault

  AnalyzeConfig
    <$> basedir
    <*> pure logSeverity
    <*> scanDestination
    <*> revisionData
    <*> modeOpts
    <*> filters
    <*> mavenScopeFilters
    <*> pure experimentalCfgs
    <*> vendoredDepsOptions
    <*> pure analyzeUnpackArchives
    <*> pure analyzeJsonOutput
    <*> pure analyzeIncludeAllDeps
    <*> pure analyzeNoDiscoveryExclusion
    <*> pure dynamicAnalysisOverrides
    <*> pure firstPartyScansFlag
    <*> pure grepOptions
    <*> pure customFossaDepsFile
    <*> pure allowedTacticType

collectMavenScopeFilters ::
  ( Has Diagnostics sig m
  ) =>
  Maybe ConfigFile ->
  m MavenScopeFilters
collectMavenScopeFilters maybeConfig =
  pure $ maybe (MavenScopeIncludeFilters mempty) collectConfigMavenScopeFilters maybeConfig

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
collectExperimental maybeCfg AnalyzeCliOpts{analyzeDynamicGoAnalysisType = goDynamicAnalysisType, analyzePathDependencies = shouldAnalyzePathDependencies} =
  ExperimentalAnalyzeConfig
    ( fmap
        gradleConfigsOnly
        (maybeCfg >>= configExperimental >>= gradle)
    )
    goDynamicAnalysisType
    shouldAnalyzePathDependencies

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

collectGrepOptions :: Maybe ConfigFile -> AnalyzeCliOpts -> GrepOptions
collectGrepOptions maybeCfg AnalyzeCliOpts{..} =
  case maybeCfg of
    Nothing -> GrepOptions [] [] orgWideCustomLicenseScanConfigPolicyFromFlag Nothing
    Just cfg ->
      GrepOptions customLicenseList keywordSearchList (orgWideCustomLicenseScanConfigPolicyFromFlag <> orgWideCustomLicenseConfigPolicyFromConfig) (configConfigFilePath <$> maybeCfg)
      where
        customLicenseList = maybe [] (map configGrepToGrep) (configCustomLicenseSearch cfg)
        keywordSearchList = maybe [] (map configGrepToGrep) (configKeywordSearch cfg)
        orgWideCustomLicenseConfigPolicyFromConfig = configOrgWideCustomLicenseConfigPolicy cfg
  where
    orgWideCustomLicenseScanConfigPolicyFromFlag = if (fromFlag IgnoreOrgWideCustomLicenseScanConfigs analyzeIgnoreOrgWideCustomLicenseScanConfigs) then Ignore else Use

configGrepToGrep :: ConfigGrepEntry -> GrepEntry
configGrepToGrep configGrep = GrepEntry (configGrepMatchCriteria configGrep) (configGrepName configGrep)

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
      metaMerged <- maybe (pure analyzeMetadata) (mergeFileCmdMetadata analyzeMetadata) (maybeCfgFile)
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
