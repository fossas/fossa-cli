{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.VPS (
  mkSubCommand,
  VPSCliOpts,
  VPSConfig (..),
  AnalyzeConfig (..),
  TestConfig (..),
  AOSPNoticeConfig (..),
  ReportConfig (..),
  OutputFormat (..),
  FollowSymlinks (..),
  LicenseOnlyScan (..),
  SkipIPRScan (..),
  ReportType (..),
) where

import App.Fossa.Config.Common (
  CacheAction (ReadOnly, WriteOnly),
  CommonOpts (..),
  baseDirArg,
  collectApiOpts,
  collectBaseDir,
  collectRevisionData',
  commonOpts,
  defaultTimeoutDuration,
  metadataOpts,
 )
import App.Fossa.Config.ConfigFile (
  ConfigFile,
  mergeFileCmdMetadata,
  resolveConfigFile,
 )
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Subcommand (
  EffStack,
  GetSeverity (..),
  SubCommand (SubCommand),
 )
import App.Fossa.VPS.Types (
  FilterExpressions (FilterExpressions),
  NinjaFilePaths (NinjaFilePaths),
  NinjaScanID (NinjaScanID),
 )
import App.OptionExtensions (jsonOption)
import App.Types (
  BaseDir,
  OverrideProject (OverrideProject),
  ProjectMetadata,
  ProjectRevision,
 )
import App.Util (validateFile)
import Control.Effect.Diagnostics (
  Diagnostics,
  fatalOnIOException,
  fatalText,
 )
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Timeout (Duration (Seconds))
import Data.Flag (Flag, flagOpt)
import Data.String.Conversion (ToString (toString))
import Data.Text (Text)
import Data.Text qualified as Text
import Effect.Exec (Exec)
import Effect.Logger (Logger, Severity (SevDebug, SevInfo))
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts)
import Options.Applicative (
  InfoMod,
  Parser,
  argument,
  auto,
  command,
  flag,
  help,
  hsubparser,
  info,
  long,
  maybeReader,
  metavar,
  option,
  optional,
  progDesc,
  short,
  strOption,
  switch,
  value,
 )
import Path (Abs, File, Path)
import Path.IO (getCurrentDir)

mkSubCommand :: (VPSConfig -> EffStack ()) -> SubCommand VPSCliOpts VPSConfig
mkSubCommand = SubCommand "vps" vpsInfo cliParser loadConfig mergeOpts

vpsInfo :: InfoMod a
vpsInfo = progDesc "Run in Vendored Package Scan mode"

data FollowSymlinks = FollowSymlinks
data LicenseOnlyScan = LicenseOnlyScan
data SkipIPRScan = SkipIPRScan

data OutputFormat
  = TestOutputJson
  | TestOutputPretty
  deriving (Eq, Ord, Show)

data ReportOutputFormat
  = ReportJson
  -- ReportPretty
  deriving (Eq, Ord, Show)

data ReportType = Attribution deriving (Eq, Ord)

instance Show ReportType where
  show Attribution = "attribution"

loadConfig ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  VPSCliOpts ->
  m (Maybe ConfigFile)
loadConfig opts = do
  curdir <- sendIO getCurrentDir
  resolveConfigFile curdir . optConfig $ getCommons opts

mergeOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  VPSCliOpts ->
  m (VPSConfig)
mergeOpts cfgfile envvars = \case
  VPSAnalyzeCommand opts -> AnalyzeCfg <$> mergeAnalyzeOpts cfgfile envvars opts
  VPSAOSPNoticeCommand opts -> AOSPNoticeCfg <$> mergeAOSPOpts cfgfile envvars opts
  VPSTestCommand opts -> TestCfg <$> mergeTestOpts cfgfile envvars opts
  VPSReportCommand opts -> ReportCfg <$> mergeReportOpts cfgfile envvars opts

mergeAnalyzeOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  VPSAnalyzeOpts ->
  m AnalyzeConfig
mergeAnalyzeOpts cfgfile envvars cliOpts@VPSAnalyzeOpts{..} = do
  let metadata = collectProjectMetadata cfgfile analyzeCliMetadata
      severity = getSeverity $ VPSAnalyzeCommand cliOpts
      filters = analyzeCliFileFilter
      followSymlinks = analyzeCliFollowSymlinks
      licenseOnly = analyzeCliLicenseOnlyScan
      skipIPR = analyzeCliSkipIprScan
  let apiopts = collectApiOpts cfgfile envvars analyzeCliCommons
  let basedir = collectBaseDir analyzeCliBaseDir
  let revision =
        collectRevisionData' basedir cfgfile WriteOnly $
          OverrideProject
            (optProjectName analyzeCliCommons)
            (optProjectRevision analyzeCliCommons)
            Nothing
  AnalyzeConfig
    <$> apiopts
    <*> basedir
    <*> pure metadata
    <*> revision
    <*> pure severity
    <*> pure filters
    <*> pure followSymlinks
    <*> pure licenseOnly
    <*> pure skipIPR

mergeAOSPOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  VPSAOSPNoticeOpts ->
  m AOSPNoticeConfig
mergeAOSPOpts cfgfile envvars cliOpts@VPSAOSPNoticeOpts{..} = do
  let metadata = collectProjectMetadata cfgfile aospNinjaScanMeta
      severity = getSeverity $ VPSAOSPNoticeCommand cliOpts
      scanId = NinjaScanID aospNinjaScanID
      apiopts = collectApiOpts cfgfile envvars aospCommons
      basedir = collectBaseDir aospCliBaseDir
      filepaths = NinjaFilePaths <$> parseCommaSeparatedFileArg aospNinjaFileList
      revision =
        collectRevisionData' basedir cfgfile WriteOnly $
          OverrideProject
            (optProjectName aospCommons)
            (optProjectRevision aospCommons)
            Nothing
  AOSPNoticeConfig
    <$> apiopts
    <*> basedir
    <*> pure metadata
    <*> revision
    <*> pure severity
    <*> filepaths
    <*> pure scanId

parseCommaSeparatedFileArg :: (Has Diagnostics sig m, Has (Lift IO) sig m) => Text -> m [Path Abs File]
parseCommaSeparatedFileArg arg =
  fatalOnIOException "Parsing comma-separated file paths" $
    traverse (sendIO . validateFile . toString) $ Text.splitOn "," arg

mergeTestOpts ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  VPSTestOpts ->
  m TestConfig
mergeTestOpts cfgfile envvars VPSTestOpts{..} = do
  let outputFormat = vpsTestOutputType
      timeoutDuration = maybe defaultTimeoutDuration Seconds vpsTestTimeout
      apiopts = collectApiOpts cfgfile envvars testCommons
      basedir = collectBaseDir vpsTestBaseDir
      revision =
        collectRevisionData' basedir cfgfile ReadOnly $
          OverrideProject
            (optProjectName testCommons)
            (optProjectRevision testCommons)
            Nothing
  TestConfig
    <$> apiopts
    <*> basedir
    <*> pure outputFormat
    <*> revision
    <*> pure timeoutDuration

mergeReportOpts ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  VPSReportOpts ->
  m ReportConfig
mergeReportOpts cfgfile envvars VPSReportOpts{..} = do
  let reportTyp = vpsReportType
      timeoutDuration = maybe defaultTimeoutDuration Seconds vpsReportTimeout
      apiopts = collectApiOpts cfgfile envvars reportCommons
      basedir = collectBaseDir vpsReportBaseDir
      outputFormat = validateReportOutputFormat vpsReportJsonOutput
      revision =
        collectRevisionData' basedir cfgfile ReadOnly $
          OverrideProject
            (optProjectName reportCommons)
            (optProjectRevision reportCommons)
            Nothing
  ReportConfig
    <$> apiopts
    <*> basedir
    <*> outputFormat
    <*> revision
    <*> pure timeoutDuration
    <*> pure reportTyp

validateReportOutputFormat :: Has Diagnostics sig m => Bool -> m ReportOutputFormat
validateReportOutputFormat dojson =
  if dojson
    then pure ReportJson
    else fatalText "Plaintext reports are not available yet"

getCommons :: VPSCliOpts -> CommonOpts
getCommons = \case
  VPSAnalyzeCommand VPSAnalyzeOpts{..} -> analyzeCliCommons
  VPSAOSPNoticeCommand VPSAOSPNoticeOpts{..} -> aospCommons
  VPSTestCommand VPSTestOpts{..} -> testCommons
  VPSReportCommand VPSReportOpts{..} -> reportCommons

collectProjectMetadata :: Maybe ConfigFile -> ProjectMetadata -> ProjectMetadata
collectProjectMetadata cfgfile cliMetadata = maybe cliMetadata (mergeFileCmdMetadata cliMetadata) cfgfile

instance GetSeverity VPSCliOpts where
  getSeverity opts = if debugMode then SevDebug else SevInfo
    where
      debugMode = optDebug $ getCommons opts

data VPSConfig
  = AnalyzeCfg AnalyzeConfig
  | AOSPNoticeCfg AOSPNoticeConfig
  | TestCfg TestConfig
  | ReportCfg ReportConfig
  deriving (Eq, Ord, Show)

data AnalyzeConfig = AnalyzeConfig
  { analyzeApiOpts :: ApiOpts
  , analyzeBaseDir :: BaseDir
  , analyzeMetadata :: ProjectMetadata
  , analyzeRevision :: ProjectRevision
  , analyzeSeverity :: Severity
  , fileFilters :: FilterExpressions
  , followSymlinks :: Flag FollowSymlinks
  , licenseOnlyScan :: Flag LicenseOnlyScan
  , skipIPRScan :: Flag SkipIPRScan
  }
  deriving (Eq, Ord, Show)

data AOSPNoticeConfig = AOSPNoticeConfig
  { aospApiOpts :: ApiOpts
  , aospBaseDir :: BaseDir
  , aospMetadata :: ProjectMetadata
  , aospRevision :: ProjectRevision
  , aospSeverity :: Severity
  , ninjaFileList :: NinjaFilePaths
  , ninjaScanId :: NinjaScanID
  }
  deriving (Eq, Ord, Show)

data TestConfig = TestConfig
  { testApiOpts :: ApiOpts
  , testBaseDir :: BaseDir
  , testOutputFormat :: OutputFormat
  , testRevision :: ProjectRevision
  , testTimeoutDuration :: Duration
  }
  deriving (Eq, Ord, Show)

data ReportConfig = ReportConfig
  { reportApiOpts :: ApiOpts
  , reportBaseDir :: BaseDir
  , reportOutputFormat :: ReportOutputFormat
  , reportRevision :: ProjectRevision
  , reportTimeoutDuration :: Duration
  , reportType :: ReportType
  }
  deriving (Eq, Ord, Show)

data VPSCliOpts
  = VPSAnalyzeCommand VPSAnalyzeOpts
  | VPSAOSPNoticeCommand VPSAOSPNoticeOpts
  | VPSTestCommand VPSTestOpts
  | VPSReportCommand VPSReportOpts
  deriving (Eq, Ord, Show)

data VPSAnalyzeOpts = VPSAnalyzeOpts
  { analyzeCliCommons :: CommonOpts
  , analyzeCliFollowSymlinks :: Flag FollowSymlinks
  , analyzeCliSkipIprScan :: Flag SkipIPRScan
  , analyzeCliLicenseOnlyScan :: Flag LicenseOnlyScan
  , analyzeCliFileFilter :: FilterExpressions
  , analyzeCliBaseDir :: FilePath
  , analyzeCliMetadata :: ProjectMetadata
  }
  deriving (Eq, Ord, Show)

data VPSAOSPNoticeOpts = VPSAOSPNoticeOpts
  { aospCommons :: CommonOpts
  , aospCliBaseDir :: FilePath
  , aospNinjaScanID :: Text
  , aospNinjaFileList :: Text
  , aospNinjaScanMeta :: ProjectMetadata
  }
  deriving (Eq, Ord, Show)

data VPSTestOpts = VPSTestOpts
  { testCommons :: CommonOpts
  , vpsTestTimeout :: Maybe Int
  , vpsTestOutputType :: OutputFormat
  , vpsTestBaseDir :: FilePath
  }
  deriving (Eq, Ord, Show)

data VPSReportOpts = VPSReportOpts
  { reportCommons :: CommonOpts
  , vpsReportJsonOutput :: Bool
  , vpsReportTimeout :: Maybe Int
  , vpsReportType :: ReportType
  , vpsReportBaseDir :: FilePath
  }
  deriving (Eq, Ord, Show)

cliParser :: Parser VPSCliOpts
cliParser =
  hsubparser
    ( command
        "analyze"
        ( info (VPSAnalyzeCommand <$> vpsAnalyzeOpts) $
            progDesc "Scan for projects and their vendored dependencies"
        )
        <> command
          "test"
          ( info (VPSTestCommand <$> vpsTestOpts) $
              progDesc "Check for issues from FOSSA and exit non-zero when issues are found"
          )
        <> command
          "report"
          ( info
              (VPSReportCommand <$> vpsReportOpts)
              (progDesc "Access various reports from FOSSA and print to stdout")
          )
        <> command
          "aosp-notice-file"
          ( info
              (VPSAOSPNoticeCommand <$> vpsAospNoticeOpts)
              (progDesc "Upload information required to generate NOTICE files for this build to FOSSA")
          )
    )

vpsAnalyzeOpts :: Parser VPSAnalyzeOpts
vpsAnalyzeOpts =
  VPSAnalyzeOpts
    <$> commonOpts
    <*> flagOpt FollowSymlinks (long "follow" <> help "If specified, follows symbolic links (does not protect against cyclic links)")
    <*> flagOpt SkipIPRScan (long "skip-ipr-scan" <> help "If specified, the scan directory will not be scanned for intellectual property rights information")
    <*> flagOpt LicenseOnlyScan (long "license-only" <> help "If specified, the scan directory will not be scanned for vendored dependencies")
    <*> (FilterExpressions <$> jsonOption (long "ignore-file-regex" <> short 'i' <> metavar "REGEXPS" <> help "JSON encoded array of regular expressions used to filter scanned paths" <> value []))
    <*> baseDirArg
    <*> metadataOpts

vpsReportOpts :: Parser VPSReportOpts
vpsReportOpts =
  VPSReportOpts
    <$> commonOpts
    <*> switch (long "json" <> help "Output the report in JSON format (Currently required).")
    <*> optional (option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)"))
    <*> reportTypeArg
    <*> baseDirArg

reportTypeArg :: Parser ReportType
reportTypeArg = argument (maybeReader parseType) (metavar "REPORT" <> help "The report type to fetch from the server.")
  where
    parseType = \case
      "attribution" -> Just Attribution
      _ -> Nothing

vpsAospNoticeOpts :: Parser VPSAOSPNoticeOpts
vpsAospNoticeOpts =
  VPSAOSPNoticeOpts
    <$> commonOpts
    <*> baseDirArg
    <*> strOption (long "scan-id" <> help "ID of the scan to which notice content should be added. Reported by `analyze` upon completion.")
    <*> strOption (long "ninja-files" <> help "A comma-separated list of ninja files to parse for build graph information.")
    <*> metadataOpts

vpsTestOpts :: Parser VPSTestOpts
vpsTestOpts =
  VPSTestOpts
    <$> commonOpts
    <*> optional (option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)"))
    <*> flag TestOutputPretty TestOutputJson (long "json" <> help "Output issues as json")
    <*> baseDirArg
