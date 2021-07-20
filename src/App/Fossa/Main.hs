{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Main (
  appMain,
) where

import App.Fossa.Analyze (JsonOutput (..), RecordMode (..), ScanDestination (..), UnpackArchives (..), VSIAnalysisMode (..), analyzeMain)
import App.Fossa.Compatibility (Argument, argumentParser, compatibilityMain)
import App.Fossa.Configuration
import App.Fossa.Container (ImageText (..), dumpSyftScanMain, imageTextArg, parseSyftOutputMain)
import App.Fossa.Container.Analyze qualified as ContainerAnalyze
import App.Fossa.Container.Test qualified as ContainerTest
import App.Fossa.EmbeddedBinary qualified as Embed
import App.Fossa.ListTargets (listTargetsMain)
import App.Fossa.Monorepo
import App.Fossa.Report qualified as Report
import App.Fossa.Test qualified as Test
import App.Fossa.VPS.AOSPNotice (aospNoticeMain)
import App.Fossa.VPS.NinjaGraph
import App.Fossa.VPS.Report qualified as VPSReport
import App.Fossa.VPS.Scan (FollowSymlinks (..), LicenseOnlyScan (..), SkipIPRScan (..), scanMain)
import App.Fossa.VPS.Test qualified as VPSTest
import App.Fossa.VPS.Types (FilterExpressions (..), NinjaFilePaths (..), NinjaScanID (..))
import App.OptionExtensions
import App.Types
import App.Util (validateDir, validateFile)
import App.Version (fullVersionDescription)
import Control.Monad (unless, when)
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Flag (Flag, flagOpt, fromFlag)
import Data.Foldable (for_)
import Data.Functor.Extra ((<$$>))
import Data.Text (Text)
import Data.Text qualified as T
import Discovery.Filters (BuildTargetFilter (..), filterParser)
import Effect.Logger
import Fossa.API.Types (ApiKey (..), ApiOpts (..))
import Options.Applicative
import Path
import System.Environment (lookupEnv)
import System.Exit (die)
import System.Info qualified as SysInfo
import Text.Megaparsec (errorBundlePretty, runParser)
import Text.URI (URI, mkURI)

windowsOsName :: String
windowsOsName = "mingw32"

mainPrefs :: ParserPrefs
mainPrefs =
  prefs $
    mconcat
      [ helpShowGlobals
      , showHelpOnError
      , subparserInline
      ]

mergeFileCmdConfig :: CmdOptions -> ConfigFile -> CmdOptions
mergeFileCmdConfig cmd file =
  CmdOptions
    { optDebug = optDebug cmd
    , optBaseUrl = optBaseUrl cmd <|> (configServer file >>= mkURI)
    , optProjectName = optProjectName cmd <|> (configProject file >>= configProjID)
    , optProjectRevision = optProjectRevision cmd <|> (configRevision file >>= configCommit)
    , optAPIKey = optAPIKey cmd <|> configApiKey file
    , optCommand = optCommand cmd
    }

appMain :: IO ()
appMain = do
  cmdConfig <- customExecParser mainPrefs (info (opts <**> helper) (fullDesc <> header "fossa-cli - Flexible, performant dependency analysis"))
  fileConfig <- readConfigFileIO

  let CmdOptions{..} = maybe cmdConfig (mergeFileCmdConfig cmdConfig) fileConfig

  let logSeverity = bool SevInfo SevDebug optDebug

  maybeApiKey <- checkAPIKey optAPIKey
  let override =
        OverrideProject
          { overrideName = optProjectName
          , overrideRevision = optProjectRevision
          , overrideBranch = Nothing
          }

  case optCommand of
    AnalyzeCommand AnalyzeOptions{analyzeOutput, analyzeBranch, analyzeMetadata, monorepoAnalysisOpts = (MonorepoAnalysisOpts (Just monorepoAnalysisType)), analyzeBaseDir} -> do
      dieOnWindows "Monorepo analysis is not supported on Windows"
      if analyzeOutput
        then die "Monorepo analysis does not support the `--output` flag"
        else do
          key <- requireKey maybeApiKey
          let apiOpts = ApiOpts optBaseUrl key
          let metadata = maybe analyzeMetadata (mergeFileCmdMetadata analyzeMetadata) fileConfig
          let monorepoAnalysisOpts = MonorepoAnalysisOpts (Just monorepoAnalysisType)
          let analyzeOverride = override{overrideBranch = analyzeBranch <|> ((fileConfig >>= configRevision) >>= configBranch)}
          basedir <- parseAbsDir analyzeBaseDir
          monorepoMain (BaseDir basedir) monorepoAnalysisOpts logSeverity apiOpts metadata analyzeOverride
    --
    AnalyzeCommand AnalyzeOptions{..} -> do
      -- The branch override needs to be set here rather than above to preserve
      -- the preference for command line options.
      let analyzeOverride = override{overrideBranch = analyzeBranch <|> ((fileConfig >>= configRevision) >>= configBranch)}
          doAnalyze destination = analyzeMain analyzeBaseDir analyzeRecordMode logSeverity destination analyzeOverride analyzeUnpackArchives analyzeJsonOutput analyzeVSIMode analyzeBuildTargetFilters

      if analyzeOutput
        then doAnalyze OutputStdout
        else do
          key <- requireKey maybeApiKey
          let apiOpts = ApiOpts optBaseUrl key
          let metadata = maybe analyzeMetadata (mergeFileCmdMetadata analyzeMetadata) fileConfig
          case (projectReleaseGroupName metadata, projectReleaseGroupRelease metadata) of
            (Just _, Just _) -> pure ()
            (Nothing, Nothing) -> pure ()
            _ -> die "releaseGroup.release and releaseGroup.name must both be specified if you want to associate this project to a release group."

          doAnalyze (UploadScan apiOpts metadata)
    --
    TestCommand TestOptions{..} -> do
      baseDir <- validateDir testBaseDir
      key <- requireKey maybeApiKey
      let apiOpts = ApiOpts optBaseUrl key
      Test.testMain baseDir apiOpts logSeverity testTimeout testOutputType override
    --
    InitCommand ->
      withDefaultLogger logSeverity $ logWarn "This command has been deprecated and is no longer needed.  It has no effect and may be safely removed."
    --
    ReportCommand ReportOptions{..} -> do
      unless reportJsonOutput $ die "report command currently only supports JSON output.  Please try `fossa report --json REPORT_NAME`"
      baseDir <- validateDir reportBaseDir
      key <- requireKey maybeApiKey
      let apiOpts = ApiOpts optBaseUrl key
      Report.reportMain baseDir apiOpts logSeverity reportTimeout reportType override
    --
    ListTargetsCommand dir -> do
      baseDir <- validateDir dir
      listTargetsMain baseDir
    --
    VPSCommand VPSOptions{..} -> do
      apikey <- requireKey maybeApiKey
      let apiOpts = ApiOpts optBaseUrl apikey
      withDefaultLogger logSeverity $ logWarn "vps commands are deprecated and will be removed in a future version. Please contact FOSSA for migration steps."
      case vpsCommand of
        VPSAnalyzeCommand VPSAnalyzeOptions{..} -> do
          when (SysInfo.os == windowsOsName) $ unless (fromFlag SkipIPRScan skipIprScan) $ die "Windows VPS scans require skipping IPR.  Please try `fossa vps analyze --skip-ipr-scan DIR`"
          baseDir <- validateDir vpsAnalyzeBaseDir
          let metadata = maybe vpsAnalyzeMeta (mergeFileCmdMetadata vpsAnalyzeMeta) fileConfig
          scanMain baseDir apiOpts metadata logSeverity override vpsFileFilter followSymlinks skipIprScan licenseOnlyScan
        NinjaGraphCommand ninjaGraphOptions -> do
          _ <- die "This command is no longer supported"
          ninjaGraphMain apiOpts logSeverity override ninjaGraphOptions
        VPSTestCommand VPSTestOptions{..} -> do
          baseDir <- validateDir vpsTestBaseDir
          VPSTest.testMain baseDir apiOpts logSeverity vpsTestTimeout vpsTestOutputType override
        VPSReportCommand VPSReportOptions{..} -> do
          unless vpsReportJsonOutput $ die "report command currently only supports JSON output.  Please try `fossa report --json REPORT_NAME`"
          baseDir <- validateDir vpsReportBaseDir
          VPSReport.reportMain baseDir apiOpts logSeverity vpsReportTimeout vpsReportType override
        VPSAOSPNoticeCommand VPSAOSPNoticeOptions{..} -> do
          dieOnWindows "Vendored Package Scanning (VPS)"
          baseDir <- validateDir vpsAOSPNoticeBaseDir
          ninjaPaths <- parseCommaSeparatedFileArg vpsNinjaFileList
          aospNoticeMain baseDir logSeverity override (NinjaScanID vpsNinjaScanID) (NinjaFilePaths ninjaPaths) apiOpts

    --
    ContainerCommand ContainerOptions{..} -> do
      dieOnWindows "container scanning"
      case containerCommand of
        ContainerAnalyze ContainerAnalyzeOptions{..} ->
          if containerAnalyzeOutput
            then ContainerAnalyze.analyzeMain OutputStdout logSeverity override containerAnalyzeImage
            else do
              let containerOverride = override{overrideBranch = containerBranch <|> ((fileConfig >>= configRevision) >>= configBranch)}
              apikey <- requireKey maybeApiKey
              let apiOpts = ApiOpts optBaseUrl apikey
              let metadata = maybe containerMetadata (mergeFileCmdMetadata containerMetadata) fileConfig
              ContainerAnalyze.analyzeMain (UploadScan apiOpts metadata) logSeverity containerOverride containerAnalyzeImage
        ContainerTest ContainerTestOptions{..} -> do
          apikey <- requireKey maybeApiKey
          let apiOpts = ApiOpts optBaseUrl apikey
          ContainerTest.testMain apiOpts logSeverity containerTestTimeout containerTestOutputType override containerTestImage
        ContainerParseFile path -> parseSyftOutputMain logSeverity path
        ContainerDumpScan ContainerDumpScanOptions{..} -> dumpSyftScanMain logSeverity dumpScanOutputFile dumpScanImage
    --
    CompatibilityCommand args -> do
      compatibilityMain args
    --
    DumpBinsCommand dir -> do
      basedir <- validateDir dir
      for_ Embed.allBins $ Embed.dumpEmbeddedBinary $ unBaseDir basedir

dieOnWindows :: String -> IO ()
dieOnWindows op = when (SysInfo.os == windowsOsName) $ die $ "Operation is not supported on Windows: " <> op

parseCommaSeparatedFileArg :: Text -> IO [Path Abs File]
parseCommaSeparatedFileArg arg = sequence (validateFile . T.unpack <$> T.splitOn "," arg)

requireKey :: Maybe ApiKey -> IO ApiKey
requireKey (Just key) = pure key
requireKey Nothing = die "A FOSSA API key is required to run this command"

-- | Try to fetch FOSSA_API_KEY from env if not supplied from cmdline
checkAPIKey :: Maybe Text -> IO (Maybe ApiKey)
checkAPIKey key = case key of
  Just key' -> pure . Just $ ApiKey key'
  Nothing -> ApiKey . T.pack <$$> lookupEnv "FOSSA_API_KEY"

baseDirArg :: Parser String
baseDirArg = argument str (metavar "DIR" <> help "Set the base directory for scanning (default: current directory)" <> value ".")

opts :: Parser CmdOptions
opts =
  CmdOptions
    <$> switch (long "debug" <> help "Enable debug logging")
    <*> optional (uriOption (long "endpoint" <> short 'e' <> metavar "URL" <> help "The FOSSA API server base URL (default: https://app.fossa.com)"))
    <*> optional (strOption (long "project" <> short 'p' <> help "this repository's URL or VCS endpoint (default: VCS remote 'origin')"))
    <*> optional (strOption (long "revision" <> short 'r' <> help "this repository's current revision hash (default: VCS hash HEAD)"))
    <*> optional (strOption (long "fossa-api-key" <> help "the FOSSA API server authentication key (default: FOSSA_API_KEY from env)"))
    <*> (commands <|> hiddenCommands)
    <**> infoOption (T.unpack fullVersionDescription) (long "version" <> short 'V' <> help "show version text")

commands :: Parser Command
commands =
  hsubparser
    ( command
        "analyze"
        ( info
            (AnalyzeCommand <$> analyzeOpts)
            (progDesc "Scan for projects and their dependencies")
        )
        <> command
          "test"
          ( info
              (TestCommand <$> testOpts)
              (progDesc "Check for issues from FOSSA and exit non-zero when issues are found")
          )
        <> command
          "report"
          ( info
              (ReportCommand <$> reportOpts)
              (progDesc "Access various reports from FOSSA and print to stdout")
          )
        <> command
          "list-targets"
          ( info
              (ListTargetsCommand <$> baseDirArg)
              (progDesc "List available analysis-targets in a directory (projects and subprojects)")
          )
        <> command
          "vps"
          ( info
              (VPSCommand <$> vpsOpts)
              (progDesc "Run in Vendored Package Scan mode")
          )
        <> command
          "container"
          ( info
              (ContainerCommand <$> containerOpts)
              (progDesc "Run in Container Scan mode")
          )
    )

hiddenCommands :: Parser Command
hiddenCommands =
  subparser
    ( internal
        <> command
          "init"
          ( info
              (pure InitCommand)
              (progDesc "Deprecated, has no effect.")
          )
        <> command
          "dump-binaries"
          ( info
              (DumpBinsCommand <$> baseDirArg)
              (progDesc "Output all embedded binaries to specified path")
          )
        <> command
          "compatibility"
          ( info
              (CompatibilityCommand <$> compatibilityOpts)
              (progDesc "Run fossa cli v1 analyze. Supply arguments as \"fossa compatibility -- --project test\"")
          )
    )

analyzeOpts :: Parser AnalyzeOptions
analyzeOpts =
  AnalyzeOptions
    <$> switch (long "output" <> short 'o' <> help "Output results to stdout instead of uploading to fossa")
    <*> flagOpt UnpackArchives (long "unpack-archives" <> help "Recursively unpack and analyze discovered archives")
    <*> flagOpt JsonOutput (long "json" <> help "Output project metadata as json to the console. Useful for communicating with the FOSSA API")
    <*> optional (strOption (long "branch" <> short 'b' <> help "this repository's current branch (default: current VCS branch)"))
    <*> metadataOpts
    <*> many filterOpt
    <*> vsiAnalyzeOpt
    <*> monorepoOpts
    <*> analyzeReplayOpt
    <*> baseDirArg

vsiAnalyzeOpt :: Parser VSIAnalysisMode
vsiAnalyzeOpt =
  flag' VSIAnalysisEnabled (long "enable-vsi" <> hidden)
    <|> pure VSIAnalysisDisabled

analyzeReplayOpt :: Parser RecordMode
analyzeReplayOpt =
  flag' RecordModeRecord (long "record" <> hidden)
    <|> (RecordModeReplay <$> strOption (long "replay" <> hidden))
    <|> pure RecordModeNone

filterOpt :: Parser BuildTargetFilter
filterOpt = option (eitherReader parseFilter) (long "filter" <> help "Analysis-Target filters (default: none)" <> metavar "ANALYSIS-TARGET")
  where
    parseFilter :: String -> Either String BuildTargetFilter
    parseFilter = first errorBundlePretty . runParser filterParser "stdin" . T.pack

monorepoOpts :: Parser MonorepoAnalysisOpts
monorepoOpts =
  MonorepoAnalysisOpts
    <$> optional (strOption (long "experimental-enable-monorepo" <> metavar "MODE" <> help "scan the project in the experimental monorepo mode. Supported modes: aosp"))

metadataOpts :: Parser ProjectMetadata
metadataOpts =
  ProjectMetadata
    <$> optional (strOption (long "title" <> short 't' <> help "the title of the FOSSA project. (default: the project name)"))
    <*> optional (strOption (long "project-url" <> short 'P' <> help "this repository's home page"))
    <*> optional (strOption (long "jira-project-key" <> short 'j' <> help "this repository's JIRA project key"))
    <*> optional (strOption (long "link" <> short 'L' <> help "a link to attach to the current build"))
    <*> optional (strOption (long "team" <> short 'T' <> help "this repository's team inside your organization"))
    <*> optional (strOption (long "policy" <> help "the policy to assign to this project in FOSSA"))
    <*> optional (strOption (long "release-group-name" <> help "the name of the release group to add this project to"))
    <*> optional (strOption (long "release-group-release" <> help "the release of the release group to add this project to"))

reportOpts :: Parser ReportOptions
reportOpts =
  ReportOptions
    <$> switch (long "json" <> help "Output the report in JSON format (Currently required).")
    <*> option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)" <> value 600)
    <*> reportCmd
    <*> baseDirArg

-- FIXME: make report type a positional argument, rather than a subcommand
reportCmd :: Parser Report.ReportType
reportCmd =
  hsubparser $
    command "attribution" (info (pure Report.AttributionReport) $ progDesc "Generate attribution report")

testOpts :: Parser TestOptions
testOpts =
  TestOptions
    <$> option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)" <> value 600)
    <*> flag Test.TestOutputPretty Test.TestOutputJson (long "json" <> help "Output issues as json")
    <*> baseDirArg

vpsOpts :: Parser VPSOptions
vpsOpts = VPSOptions <$> followSymlinksOpt <*> skipIprScanOpt <*> licenseOnlyScanOpt <*> fileFilterOpt <*> vpsCommands
  where
    followSymlinksOpt = flagOpt FollowSymlinks (long "follow" <> help "If specified, follows symbolic links (does not protect against cyclic links)")
    skipIprScanOpt = flagOpt SkipIPRScan (long "skip-ipr-scan" <> help "If specified, the scan directory will not be scanned for intellectual property rights information")
    licenseOnlyScanOpt = flagOpt LicenseOnlyScan (long "license-only" <> help "If specified, the scan directory will not be scanned for vendored dependencies")
    fileFilterOpt = FilterExpressions <$> jsonOption (long "ignore-file-regex" <> short 'i' <> metavar "REGEXPS" <> help "JSON encoded array of regular expressions used to filter scanned paths" <> value [])

vpsAnalyzeOpts :: Parser VPSAnalyzeOptions
vpsAnalyzeOpts = VPSAnalyzeOptions <$> baseDirArg <*> metadataOpts

vpsReportOpts :: Parser VPSReportOptions
vpsReportOpts =
  VPSReportOptions
    <$> switch (long "json" <> help "Output the report in JSON format (Currently required).")
    <*> option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)" <> value 600)
    <*> vpsReportCmd
    <*> baseDirArg

vpsAospNoticeOpts :: Parser VPSAOSPNoticeOptions
vpsAospNoticeOpts =
  VPSAOSPNoticeOptions
    <$> baseDirArg
    <*> strOption (long "scan-id" <> help "ID of the scan to which notice content should be added. Reported by `analyze` upon completion.")
    <*> strOption (long "ninja-files" <> help "A comma-separated list of ninja files to parse for build graph information.")
    <*> metadataOpts

-- FIXME: make report type a positional argument, rather than a subcommand
vpsReportCmd :: Parser VPSReport.ReportType
vpsReportCmd =
  hsubparser $
    command "attribution" (info (pure VPSReport.AttributionReport) $ progDesc "Generate attribution report")

vpsTestOpts :: Parser VPSTestOptions
vpsTestOpts =
  VPSTestOptions
    <$> option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)" <> value 600)
    <*> flag VPSTest.TestOutputPretty VPSTest.TestOutputJson (long "json" <> help "Output issues as json")
    <*> baseDirArg

ninjaGraphOpts :: Parser NinjaGraphCLIOptions
ninjaGraphOpts = NinjaGraphCLIOptions <$> baseDirArg <*> ninjaDepsOpt <*> lunchTargetOpt <*> scanIdOpt <*> buildNameOpt
  where
    ninjaDepsOpt = optional $ strOption (long "ninjadeps" <> metavar "STRING")
    lunchTargetOpt = optional $ strOption (long "lunchtarget" <> metavar "STRING" <> help "Build target name to pass to lunch. If you are running in an environment with envsetup and lunch already configured, then you don't need to pass this in")
    scanIdOpt = strOption (long "scan-id" <> metavar "STRING" <> help "The scan ID that this build applies to")
    buildNameOpt = strOption (long "build-name" <> metavar "STRING" <> help "Human readable name of this build. This will be shown on the FOSSA website.")

vpsCommands :: Parser VPSCommand
vpsCommands =
  hsubparser
    ( command
        "analyze"
        ( info (VPSAnalyzeCommand <$> vpsAnalyzeOpts) $
            progDesc "Scan for projects and their vendored dependencies"
        )
        <> command
          "ninja-graph"
          ( info (NinjaGraphCommand <$> ninjaGraphOpts) $
              progDesc "Get a dependency graph for a ninja build"
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

containerOpts :: Parser ContainerOptions
containerOpts = ContainerOptions <$> (containerCommands <|> hiddenContainerCommands)

containerCommands :: Parser ContainerCommand
containerCommands =
  hsubparser
    ( command
        "analyze"
        ( info (ContainerAnalyze <$> containerAnalyzeOpts) $
            progDesc "Scan an image for vulnerabilities"
        )
        <> command
          "test"
          ( info (ContainerTest <$> containerTestOpts) $
              progDesc "Check for issues from FOSSA and exit non-zero when issues are found"
          )
    )

hiddenContainerCommands :: Parser ContainerCommand
hiddenContainerCommands =
  hsubparser
    ( internal
        <> command
          "parse-file"
          ( info (ContainerParseFile <$> containerParseFileOptions) $
              progDesc "Debug syft output parsing"
          )
        <> command
          "dump-scan"
          ( info (ContainerDumpScan <$> containerDumpScanOptions) $
              progDesc "Capture syft output for debugging"
          )
    )

containerAnalyzeOpts :: Parser ContainerAnalyzeOptions
containerAnalyzeOpts =
  ContainerAnalyzeOptions
    <$> switch (long "output" <> short 'o' <> help "Output results to stdout instead of uploading to fossa")
    <*> optional (strOption (long "branch" <> short 'b' <> help "this repository's current branch (default: current VCS branch)"))
    <*> metadataOpts
    <*> imageTextArg

containerTestOpts :: Parser ContainerTestOptions
containerTestOpts =
  ContainerTestOptions
    <$> option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)" <> value 600)
    <*> flag ContainerTest.TestOutputPretty ContainerTest.TestOutputJson (long "json" <> help "Output issues as json")
    <*> imageTextArg

containerParseFileOptions :: Parser FilePath
containerParseFileOptions = argument str (metavar "FILE" <> help "File to parse")

containerDumpScanOptions :: Parser ContainerDumpScanOptions
containerDumpScanOptions =
  ContainerDumpScanOptions
    <$> optional (strOption (short 'o' <> long "output-file" <> help "File to write the scan data (omit for stdout)"))
    <*> imageTextArg

compatibilityOpts :: Parser [Argument]
compatibilityOpts =
  many argumentParser

data CmdOptions = CmdOptions
  { optDebug :: Bool
  , optBaseUrl :: Maybe URI
  , optProjectName :: Maybe Text
  , optProjectRevision :: Maybe Text
  , optAPIKey :: Maybe Text
  , optCommand :: Command
  }

data Command
  = AnalyzeCommand AnalyzeOptions
  | TestCommand TestOptions
  | ReportCommand ReportOptions
  | VPSCommand VPSOptions
  | ContainerCommand ContainerOptions
  | CompatibilityCommand [Argument]
  | ListTargetsCommand FilePath
  | InitCommand
  | DumpBinsCommand FilePath

data VPSCommand
  = VPSAnalyzeCommand VPSAnalyzeOptions
  | VPSAOSPNoticeCommand VPSAOSPNoticeOptions
  | NinjaGraphCommand NinjaGraphCLIOptions
  | VPSTestCommand VPSTestOptions
  | VPSReportCommand VPSReportOptions

data VPSReportOptions = VPSReportOptions
  { vpsReportJsonOutput :: Bool
  , vpsReportTimeout :: Int
  , vpsReportType :: VPSReport.ReportType
  , vpsReportBaseDir :: FilePath
  }

data ReportOptions = ReportOptions
  { reportJsonOutput :: Bool
  , reportTimeout :: Int
  , reportType :: Report.ReportType
  , reportBaseDir :: FilePath
  }

data AnalyzeOptions = AnalyzeOptions
  { analyzeOutput :: Bool
  , analyzeUnpackArchives :: Flag UnpackArchives
  , analyzeJsonOutput :: Flag JsonOutput
  , analyzeBranch :: Maybe Text
  , analyzeMetadata :: ProjectMetadata
  , analyzeBuildTargetFilters :: [BuildTargetFilter]
  , analyzeVSIMode :: VSIAnalysisMode
  , monorepoAnalysisOpts :: MonorepoAnalysisOpts
  , analyzeRecordMode :: RecordMode
  , analyzeBaseDir :: FilePath
  }

data TestOptions = TestOptions
  { testTimeout :: Int
  , testOutputType :: Test.TestOutputType
  , testBaseDir :: FilePath
  }

data VPSOptions = VPSOptions
  { followSymlinks :: Flag FollowSymlinks
  , skipIprScan :: Flag SkipIPRScan
  , licenseOnlyScan :: Flag LicenseOnlyScan
  , vpsFileFilter :: FilterExpressions
  , vpsCommand :: VPSCommand
  }

data VPSAnalyzeOptions = VPSAnalyzeOptions
  { vpsAnalyzeBaseDir :: FilePath
  , vpsAnalyzeMeta :: ProjectMetadata
  }

data VPSAOSPNoticeOptions = VPSAOSPNoticeOptions
  { vpsAOSPNoticeBaseDir :: FilePath
  , vpsNinjaScanID :: Text
  , vpsNinjaFileList :: Text
  , vpsNinjaScanMeta :: ProjectMetadata
  }

data VPSTestOptions = VPSTestOptions
  { vpsTestTimeout :: Int
  , vpsTestOutputType :: VPSTest.TestOutputType
  , vpsTestBaseDir :: FilePath
  }

newtype ContainerOptions = ContainerOptions
  {containerCommand :: ContainerCommand}

data ContainerCommand
  = ContainerAnalyze ContainerAnalyzeOptions
  | ContainerTest ContainerTestOptions
  | ContainerParseFile FilePath
  | ContainerDumpScan ContainerDumpScanOptions

data ContainerAnalyzeOptions = ContainerAnalyzeOptions
  { containerAnalyzeOutput :: Bool
  , containerBranch :: Maybe Text
  , containerMetadata :: ProjectMetadata
  , containerAnalyzeImage :: ImageText
  }

data ContainerTestOptions = ContainerTestOptions
  { containerTestTimeout :: Int
  , containerTestOutputType :: ContainerTest.TestOutputType
  , containerTestImage :: ImageText
  }

data ContainerDumpScanOptions = ContainerDumpScanOptions
  { dumpScanOutputFile :: Maybe FilePath
  , dumpScanImage :: ImageText
  }
