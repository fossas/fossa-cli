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
  StaticOnlyTactics (..),
  WithoutDefaultFilters (..),
  StrictMode (..),
  ExperimentalSnippetScan (..),
  SnippetScan (..),
  mkSubCommand,
  loadConfig,
  cliParser,
  mergeOpts,
  branchHelp,
  withoutDefaultFilterParser,
) where

import App.Docs (fossaAnalyzeDefaultFilterDocUrl)
import App.Fossa.Config.Common (
  CacheAction (WriteOnly),
  CommonOpts (..),
  DestinationMeta (..),
  OutputStyle (..),
  ScanDestination (..),
  applyReleaseGroupDeprecationWarning,
  baseDirArg,
  collectApiOpts,
  collectBaseDir,
  collectConfigFileFilters,
  collectConfigMavenScopeFilters,
  collectRevisionData',
  commonOpts,
  metadataOpts,
  outputStyleArgs,
  pathOpt,
  targetOpt,
  validateDir,
  validateExists,
  validateFile,
 )
import App.Fossa.Config.ConfigFile (
  ConfigFile (..),
  ConfigGrepEntry (..),
  ConfigTargets (targetsExcludeManifestStrategies),
  ConfigTelemetryScope (NoTelemetry),
  ExperimentalConfigs (..),
  ExperimentalGradleConfigs (..),
  OrgWideCustomLicenseConfigPolicy (..),
  ReachabilityConfigFile (..),
  VendoredDependencyConfigs (..),
  mergeFileCmdMetadata,
  resolveConfigFile,
 )
import App.Fossa.Config.EnvironmentVars (EnvVars (..))
import App.Fossa.Lernie.Types (GrepEntry (..), GrepOptions (..))
import App.Fossa.Reachability.Types (ReachabilityConfig (..))
import App.Fossa.Subcommand (EffStack, GetCommonOpts (getCommonOpts), GetSeverity (getSeverity), SubCommand (SubCommand))
import App.Fossa.VSI.Types qualified as VSI
import App.Types (
  BaseDir,
  FirstPartyScansFlag (..),
  Mode (..),
  OverrideDynamicAnalysisBinary (..),
  OverrideProject (OverrideProject),
  ProjectMetadata (projectLabel),
  ProjectRevision,
 )
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  fatalText,
  recover,
 )
import Control.Effect.Lift (Lift)
import Control.Monad (void, when)
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Flag (Flag, flagOpt, fromFlag)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
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
import Effect.Logger (Logger, Severity (SevDebug, SevInfo), logInfo, logWarn, pretty, vsep)
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
import Path (Abs, Dir, File, Path, Rel)
import Path.Extra (SomePath)
import Prettyprinter (Doc, indent)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Green))
import Style (applyFossaStyle, boldItalicized, coloredBoldItalicized, formatDoc, stringToHelpDoc)
import Types (ArchiveUploadType (..), DiscoveredProjectType, LicenseScanPathFilters (..), TargetFilter (..))

-- CLI flags, for use with 'Data.Flag'
data DeprecatedAllowNativeLicenseScan = DeprecatedAllowNativeLicenseScan deriving (Generic)
data DeprecatedUseV3GoResolver = DeprecatedUseV3GoResolver deriving (Generic)
data ForceVendoredDependencyRescans = ForceVendoredDependencyRescans deriving (Generic)
data ForceFirstPartyScans = ForceFirstPartyScans deriving (Generic)
data ForceNoFirstPartyScans = ForceNoFirstPartyScans deriving (Generic)
data IgnoreOrgWideCustomLicenseScanConfigs = IgnoreOrgWideCustomLicenseScanConfigs deriving (Generic)
data StaticOnlyTactics = StaticOnlyTactics deriving (Generic)
data StrictMode = StrictMode deriving (Generic, Show)
data ExperimentalSnippetScan = ExperimentalSnippetScan deriving (Generic)
data SnippetScan = SnippetScan deriving (Generic)

data BinaryDiscovery = BinaryDiscovery deriving (Generic)
data IncludeAll = IncludeAll deriving (Generic)
data JsonOutput = JsonOutput deriving (Generic)
data NoDiscoveryExclusion = NoDiscoveryExclusion deriving (Generic)
data UnpackArchives = UnpackArchives deriving (Generic)
data VSIAnalysis = VSIAnalysis deriving (Generic)
data WithoutDefaultFilters = WithoutDefaultFilters deriving (Generic)
data ExcludeManifestStrategies = ExcludeManifestStrategies deriving (Generic)

newtype IATAssertion = IATAssertion {unIATAssertion :: Maybe (Path Abs Dir)} deriving (Eq, Ord, Show, Generic)
newtype DynamicLinkInspect = DynamicLinkInspect {unDynamicLinkInspect :: Maybe SomePath} deriving (Eq, Ord, Show, Generic)

instance ToJSON WithoutDefaultFilters where
  toEncoding = genericToEncoding defaultOptions

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

instance ToJSON ExcludeManifestStrategies where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON ExperimentalSnippetScan where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON SnippetScan where
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
  , analyzeOutput :: OutputStyle
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
  , analyzeExcludeManifestStrategies :: Flag ExcludeManifestStrategies
  , analyzeVSIMode :: Flag VSIAnalysis
  , analyzeBinaryDiscoveryMode :: Flag BinaryDiscovery
  , analyzeAssertMode :: Maybe (FilePath)
  , analyzeDynamicLinkTarget :: Maybe (FilePath)
  , analyzeSkipVSIGraphResolution :: [VSI.Locator]
  , analyzeBaseDir :: FilePath
  , analyzeDeprecatedUseV3GoResolver :: Flag DeprecatedUseV3GoResolver
  , analyzePathDependencies :: Bool
  , analyzeForceFirstPartyScans :: Flag ForceFirstPartyScans
  , analyzeForceNoFirstPartyScans :: Flag ForceNoFirstPartyScans
  , analyzeIgnoreOrgWideCustomLicenseScanConfigs :: Flag IgnoreOrgWideCustomLicenseScanConfigs
  , analyzeCustomFossaDepsFile :: Maybe FilePath
  , analyzeStaticOnlyTactics :: Flag StaticOnlyTactics
  , analyzeWithoutDefaultFilters :: Flag WithoutDefaultFilters
  , analyzeStrictMode :: Flag StrictMode
  , analyzeExperimentalSnippetScan :: Flag ExperimentalSnippetScan
  , analyzeSnippetScan :: Flag SnippetScan
  , analyzeVendetta :: Bool
  }
  deriving (Eq, Ord, Show)

instance GetCommonOpts AnalyzeCliOpts where
  getCommonOpts AnalyzeCliOpts{analyzeOutput, commons} =
    case analyzeOutput of
      Output -> Just commons{optTelemetry = Just NoTelemetry} -- When `--output` is used don't emit no telemetry.
      _ -> Just commons

instance GetSeverity AnalyzeCliOpts where
  getSeverity AnalyzeCliOpts{commons = CommonOpts{optDebug}} = if optDebug then SevDebug else SevInfo

data AnalysisTacticTypes = StaticOnly | Any deriving (Eq, Ord, Show, Generic)

instance ToJSON AnalysisTacticTypes where
  toEncoding = genericToEncoding defaultOptions

data AnalyzeConfig = AnalyzeConfig
  { baseDir :: BaseDir
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
  , reachabilityConfig :: ReachabilityConfig
  , withoutDefaultFilters :: Flag WithoutDefaultFilters
  , mode :: Mode
  , snippetScan :: Bool
  , debugDir :: Maybe FilePath
  , xVendetta :: Bool
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON AnalyzeConfig where
  toEncoding = genericToEncoding defaultOptions

data ExperimentalAnalyzeConfig = ExperimentalAnalyzeConfig
  { allowedGradleConfigs :: Maybe (Set Text)
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
    <*> outputStyleArgs
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
    <*> flagOpt ExcludeManifestStrategies (applyFossaStyle <> long "exclude-manifest-strategies" <> stringToHelpDoc "Exclude all manifest-based strategies for finding targets.")
    <*> vsiEnableOpt
    <*> flagOpt BinaryDiscovery (applyFossaStyle <> long "experimental-enable-binary-discovery" <> stringToHelpDoc "Reports binary files as unlicensed dependencies")
    <*> optional (strOption (applyFossaStyle <> long "experimental-link-project-binary" <> metavar "DIR" <> hidden))
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
    <*> withoutDefaultFilterParser fossaAnalyzeDefaultFilterDocUrl
    <*> flagOpt StrictMode (applyFossaStyle <> long "strict" <> stringToHelpDoc "Enforces strict analysis to ensure the most accurate results by rejecting fallbacks.")
    <*> flagOpt ExperimentalSnippetScan (applyFossaStyle <> long "x-snippet-scan" <> hidden)
    <*> flagOpt SnippetScan (applyFossaStyle <> long "snippet-scan" <> stringToHelpDoc "Enable snippet scanning to identify open source code snippets using fingerprinting.")
    <*> switch (applyFossaStyle <> long "x-vendetta" <> stringToHelpDoc "Experimental flag to enable vendored dependency scanning to identify open source components using file hashing.")
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

withoutDefaultFilterParser :: Text -> Parser (Flag WithoutDefaultFilters)
withoutDefaultFilterParser docsUrl = flagOpt WithoutDefaultFilters (applyFossaStyle <> long "without-default-filters" <> helpDoc helpMsg)
  where
    helpMsg :: Maybe (Doc AnsiStyle)
    helpMsg =
      Just . formatDoc $
        vsep
          [ "Ignores default filters."
          , boldItalicized "Docs: " <> pretty docsUrl
          ]

experimentalUseV3GoResolver :: Parser (Flag DeprecatedUseV3GoResolver)
experimentalUseV3GoResolver = flagOpt DeprecatedUseV3GoResolver (applyFossaStyle <> long "experimental-use-v3-go-resolver" <> hidden)

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
        , hidden
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
  Maybe FilePath ->
  Maybe ConfigFile ->
  EnvVars ->
  AnalyzeCliOpts ->
  m AnalyzeConfig
mergeOpts maybeDebugDir cfg env cliOpts = do
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

  let useV3GoResolverFlagUsed = fromFlag DeprecatedUseV3GoResolver $ analyzeDeprecatedUseV3GoResolver cliOpts
  when useV3GoResolverFlagUsed $ do
    logWarn $
      vsep
        [ "DEPRECATION NOTICE"
        , "========================"
        , "The --experimental-use-v3-go-resolver flag is deprecated and no longer has any effect."
        , ""
        , "The v3 Go resolver (package-based analysis) is now the default behavior."
        , ""
        , "Please remove this flag from your commands."
        ]

  case analyzeAssertMode cliOpts of
    Just _ ->
      logWarn $
        vsep
          [ "DEPRECATION NOTICE"
          , "========================"
          , "The --experimental-link-project-binary flag is deprecated and will be removed in a future release."
          , ""
          , "Multi-stage builds feature is being deprecated."
          , ""
          , "Please remove this flag from your commands."
          ]
    Nothing -> pure ()

  case analyzeSkipVSIGraphResolution cliOpts of
    [] -> pure ()
    _ ->
      logWarn $
        vsep
          [ "DEPRECATION NOTICE"
          , "========================"
          , "The --experimental-skip-vsi-graph flag is deprecated and will be removed in a future release."
          , ""
          , "Multi-stage builds feature is being deprecated."
          , ""
          , "Please remove this flag from your commands."
          ]

  mergeStandardOpts maybeDebugDir cfg env cliOpts

mergeStandardOpts ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  Maybe FilePath ->
  Maybe ConfigFile ->
  EnvVars ->
  AnalyzeCliOpts ->
  m AnalyzeConfig
mergeStandardOpts maybeDebugDir maybeConfig envvars cliOpts@AnalyzeCliOpts{..} = do
  let basedir = collectBaseDir analyzeBaseDir
      scanDestination = collectScanDestination maybeConfig envvars cliOpts
      revisionData =
        collectRevisionData' basedir maybeConfig WriteOnly $
          OverrideProject (optProjectName commons) (optProjectRevision commons) (analyzeBranch)
      vsiModeOpts = collectVsiModeOptions cliOpts
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
      reachabilityConfig = collectReachabilityOptions maybeConfig
      mode =
        if fromFlag StrictMode analyzeStrictMode
          then Strict
          else NonStrict

  firstPartyScansFlag <-
    case (fromFlag ForceFirstPartyScans analyzeForceFirstPartyScans, fromFlag ForceNoFirstPartyScans analyzeForceNoFirstPartyScans) of
      (True, True) -> fatalText "You provided both the --experimental-force-first-party-scans and --experimental-block-first-party-scans flags. Only one of these flags may be used"
      (True, _) -> pure FirstPartyScansOnFromFlag
      (_, True) -> pure FirstPartyScansOffFromFlag
      (False, False) -> pure FirstPartyScansUseDefault

  let experimentalSnippetScanFlagUsed = fromFlag ExperimentalSnippetScan analyzeExperimentalSnippetScan
  when experimentalSnippetScanFlagUsed $ do
    logWarn $
      vsep
        [ "DEPRECATION NOTICE"
        , "========================"
        , "The --x-snippet-scan flag is deprecated."
        , ""
        , "Please use --snippet-scan instead."
        , ""
        , "In the future, usage of the --x-snippet-scan flag may result in a fatal error."
        ]

  let snippetScanEnabled = experimentalSnippetScanFlagUsed || fromFlag SnippetScan analyzeSnippetScan

  AnalyzeConfig
    <$> basedir
    <*> scanDestination
    <*> revisionData
    <*> vsiModeOpts
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
    <*> resolveReachabilityOptions reachabilityConfig
    <*> pure analyzeWithoutDefaultFilters
    <*> pure mode
    <*> pure snippetScanEnabled
    <*> pure maybeDebugDir
    <*> pure analyzeVendetta

collectMavenScopeFilters ::
  (Has Diagnostics sig m) =>
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
collectFilters maybeConfig cliOpts@AnalyzeCliOpts{..} = do
  let cliFilters = collectCLIFilters cliOpts
      cfgFileFilters = maybe mempty collectConfigFileFilters maybeConfig

      cliExcludeManifestStrategiesFlag = fromFlag ExcludeManifestStrategies analyzeExcludeManifestStrategies
      cfgFileExcludeManifestStrategiesFlag = maybe False collectConfigExcludeManifestStrategies maybeConfig

      allProjectTypes :: [DiscoveredProjectType]
      allProjectTypes = enumFromTo minBound maxBound

      allTargetFilters = map (TypeTarget . toText) allProjectTypes
      excludeManifestStrategiesFilters = AllFilters (comboInclude [] []) (comboExclude allTargetFilters [])

      resolveFilters filters True =
        let logMessage =
              if isMempty filters
                then
                  logInfo "Excluding manifest strategies in locating targets"
                else
                  logWarn "Ignoring explicit filters because \"exclude manifest strategies\" is set to true"
         in excludeManifestStrategiesFilters <$ logMessage
      resolveFilters filters False = pure filters

  case (isMempty cliFilters && not cliExcludeManifestStrategiesFlag, isMempty cfgFileFilters && not cfgFileExcludeManifestStrategiesFlag) of
    (True, True) -> pure mempty
    (False, True) -> resolveFilters cliFilters cliExcludeManifestStrategiesFlag
    (True, False) -> resolveFilters cfgFileFilters cfgFileExcludeManifestStrategiesFlag
    (False, False) ->
      resolveFilters cliFilters cliExcludeManifestStrategiesFlag
        <* logWarn "Overriding config file filters with command-line filters"

collectConfigExcludeManifestStrategies :: ConfigFile -> Bool
collectConfigExcludeManifestStrategies configFile = maybe False targetsExcludeManifestStrategies (configTargets configFile)

collectCLIFilters :: AnalyzeCliOpts -> AllFilters
collectCLIFilters AnalyzeCliOpts{..} =
  AllFilters
    (comboInclude analyzeOnlyTargets analyzeOnlyPaths)
    (comboExclude analyzeExcludeTargets analyzeExcludePaths)

collectExperimental :: Maybe ConfigFile -> AnalyzeCliOpts -> ExperimentalAnalyzeConfig
collectExperimental maybeCfg AnalyzeCliOpts{analyzePathDependencies = shouldAnalyzePathDependencies} =
  ExperimentalAnalyzeConfig
    ( fmap
        gradleConfigsOnly
        (maybeCfg >>= configExperimental >>= gradle)
    )
    shouldAnalyzePathDependencies

collectVendoredDeps ::
  (Has Diagnostics sig m) =>
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
  (Has Diagnostics sig m) =>
  Maybe ConfigFile ->
  EnvVars ->
  AnalyzeCliOpts ->
  m ScanDestination
collectScanDestination maybeCfgFile envvars AnalyzeCliOpts{..} =
  case analyzeOutput of
    Output -> pure OutputStdout
    TeeOutput -> getScanUploadDest OutputAndUpload
    Default -> getScanUploadDest UploadScan
  where
    getScanUploadDest constructor = do
      apiOpts <- collectApiOpts maybeCfgFile envvars commons
      metaMerged <- maybe (pure analyzeMetadata) (mergeFileCmdMetadata analyzeMetadata) (maybeCfgFile)
      void $ applyReleaseGroupDeprecationWarning metaMerged
      when (length (projectLabel metaMerged) > 5) $ fatalText "Projects are only allowed to have 5 associated project labels"
      pure $ constructor (DestinationMeta (apiOpts, metaMerged))

collectVsiModeOptions ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  AnalyzeCliOpts ->
  m VSIModeOptions
collectVsiModeOptions AnalyzeCliOpts{..} = do
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

collectReachabilityOptions :: Maybe ConfigFile -> ReachabilityConfigFile
collectReachabilityOptions (Just ConfigFile{configReachability = (Just conf)}) = conf
collectReachabilityOptions _ = mempty

resolveReachabilityOptions ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  ReachabilityConfigFile ->
  m ReachabilityConfig
resolveReachabilityOptions cf =
  ReachabilityConfig
    . Map.fromList
    . catMaybes
    <$> traverse resolveProjectAndJars outputs
  where
    outputs :: [(String, [String])]
    outputs = Map.toList $ configFileReachabilityJvmOutputs cf

    resolveProjectAndJars :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has ReadFS sig m) => (String, [String]) -> m (Maybe (Path Abs Dir, [Path Abs File]))
    resolveProjectAndJars (projectPath, jarPaths) =
      context ("resolve provided jars for package at '" <> toText projectPath <> "'") $
        resolveProject projectPath >>= \case
          Just projectPath' -> Just . (projectPath',) . catMaybes <$> traverse resolveJar jarPaths
          Nothing -> pure Nothing

    resolveJar :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has ReadFS sig m) => String -> m (Maybe (Path Abs File))
    resolveJar jarPath = context ("resolve provided jar path '" <> toText jarPath <> "'") . recover $ validateFile jarPath

    resolveProject :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has ReadFS sig m) => String -> m (Maybe (Path Abs Dir))
    resolveProject projectPath = context "resolve provided project path" . recover $ validateDir projectPath
