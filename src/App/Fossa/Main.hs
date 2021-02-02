{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Main
  ( appMain,
  )
where

import App.Fossa.Analyze (ScanDestination (..), UnpackArchives (..), analyzeMain)
import App.Fossa.Container (imageTextArg, ImageText (..))
import qualified App.Fossa.Container.Analyze as ContainerAnalyze
import qualified App.Fossa.Container.Test as ContainerTest
import App.Fossa.Compatibility (argumentParser, Argument, compatibilityMain)
import qualified App.Fossa.EmbeddedBinary as Embed
import App.Fossa.ListTargets (listTargetsMain)
import qualified App.Fossa.Report as Report
import qualified App.Fossa.Test as Test
import App.Fossa.VPS.NinjaGraph
import qualified App.Fossa.VPS.Report as VPSReport
import App.Fossa.VPS.Scan (LicenseOnlyScan (..), SkipIPRScan (..), scanMain)
import App.Fossa.VPS.AOSPNotice (WriteEnabled (..), aospNoticeMain)
import qualified App.Fossa.VPS.Test as VPSTest
import App.Fossa.VPS.Types (FilterExpressions (..))
import App.OptionExtensions
import App.Types
import App.Util (validateDir)
import App.Version (fullVersionDescription)
import Control.Monad (unless, when)
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Flag (Flag, flagOpt)
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import Discovery.Filters (BuildTargetFilter (..), filterParser)
import Effect.Logger
import Fossa.API.Types (ApiKey(..), ApiOpts(..))
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (die)
import qualified System.Info as SysInfo
import Text.Megaparsec (errorBundlePretty, runParser)
import Text.URI (URI)
import Text.URI.QQ (uri)

windowsOsName :: String
windowsOsName = "mingw32"

appMain :: IO ()
appMain = do
  CmdOptions {..} <- customExecParser (prefs (showHelpOnError <> subparserInline)) (info (opts <**> helper) (fullDesc <> header "fossa-cli - Flexible, performant dependency analysis"))
  let logSeverity = bool SevInfo SevDebug optDebug

  maybeApiKey <- checkAPIKey optAPIKey
  let override =
        OverrideProject
          { overrideName = optProjectName,
            overrideRevision = optProjectRevision,
            overrideBranch = Nothing
          }

  case optCommand of
    AnalyzeCommand AnalyzeOptions {..} -> do
      baseDir <- validateDir analyzeBaseDir
      let analyzeOverride = override {overrideBranch = analyzeBranch}
      if analyzeOutput
        then analyzeMain baseDir logSeverity OutputStdout analyzeOverride analyzeUnpackArchives analyzeBuildTargetFilters
        else do
          key <- requireKey maybeApiKey
          let apiOpts = ApiOpts optBaseUrl key
          analyzeMain baseDir logSeverity (UploadScan apiOpts analyzeMetadata) analyzeOverride analyzeUnpackArchives analyzeBuildTargetFilters
    --
    TestCommand TestOptions {..} -> do
      baseDir <- validateDir testBaseDir
      key <- requireKey maybeApiKey
      let apiOpts = ApiOpts optBaseUrl key
      Test.testMain baseDir apiOpts logSeverity testTimeout testOutputType override
    --
    InitCommand ->
      withLogger logSeverity $ logWarn "This command has been deprecated and is no longer needed.  It has no effect and may be safely removed."
    --
    ReportCommand ReportOptions {..} -> do
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
    VPSCommand VPSOptions {..} -> do
      dieOnWindows "Vendored Package Scanning (VPS)"
      apikey <- requireKey maybeApiKey
      let apiOpts = ApiOpts optBaseUrl apikey
      case vpsCommand of
        VPSAnalyzeCommand VPSAnalyzeOptions {..} -> do
          baseDir <- validateDir vpsAnalyzeBaseDir
          scanMain baseDir apiOpts vpsAnalyzeMeta logSeverity override vpsFileFilter skipIprScan licenseOnlyScan
        NinjaGraphCommand ninjaGraphOptions -> do
          ninjaGraphMain apiOpts logSeverity override ninjaGraphOptions
        VPSTestCommand VPSTestOptions {..} -> do
          baseDir <- validateDir vpsTestBaseDir
          VPSTest.testMain baseDir apiOpts logSeverity vpsTestTimeout vpsTestOutputType override
        VPSReportCommand VPSReportOptions {..} -> do
          unless vpsReportJsonOutput $ die "report command currently only supports JSON output.  Please try `fossa report --json REPORT_NAME`"
          baseDir <- validateDir vpsReportBaseDir
          VPSReport.reportMain baseDir apiOpts logSeverity vpsReportTimeout vpsReportType override
        VPSAOSPNoticeCommand VPSAOSPNoticeOptions {..} -> do
          baseDir <- validateDir vpsAOSPNoticeBaseDir
          aospNoticeMain baseDir logSeverity vpsFileFilter vpsAOSPNoticeWriteEnabled

    --
    ContainerCommand ContainerOptions {..} -> do
      die "Fatal: Container scanning is not available yet" >> pure ()
      dieOnWindows "container scanning"
      case containerCommand of
        ContainerAnalyze ContainerAnalyzeOptions {..} ->
          if containerAnalyzeOutput
            then ContainerAnalyze.analyzeMain OutputStdout logSeverity override containerAnalyzeImage
            else do
              apikey <- requireKey maybeApiKey
              let apiOpts = ApiOpts optBaseUrl apikey
              ContainerAnalyze.analyzeMain (UploadScan apiOpts containerMetadata) logSeverity override containerAnalyzeImage
        ContainerTest ContainerTestOptions {..} -> do
          apikey <- requireKey maybeApiKey
          let apiOpts = ApiOpts optBaseUrl apikey
          ContainerTest.testMain apiOpts logSeverity containerTestTimeout containerTestOutputType override containerTestImage
    --
    CompatibilityCommand args -> do
      compatibilityMain args
    --
    DumpBinsCommand dir -> do
      basedir <- validateDir dir
      for_ Embed.allBins $ Embed.dumpEmbeddedBinary $ unBaseDir basedir


dieOnWindows :: String -> IO ()
dieOnWindows op = when (SysInfo.os == windowsOsName) $ die $ "Operation is not supported on Windows: " <> op

requireKey :: Maybe ApiKey -> IO ApiKey
requireKey (Just key) = pure key
requireKey Nothing = die "A FOSSA API key is required to run this command"

infixl 4 <$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

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
    <*> uriOption (long "endpoint" <> metavar "URL" <> help "The FOSSA API server base URL (default: https://app.fossa.com)" <> value [uri|https://app.fossa.com|])
    <*> optional (strOption (long "project" <> help "this repository's URL or VCS endpoint (default: VCS remote 'origin')"))
    <*> optional (strOption (long "revision" <> help "this repository's current revision hash (default: VCS hash HEAD)"))
    <*> optional (strOption (long "fossa-api-key" <> help "the FOSSA API server authenticaion key (default: FOSSA_API_KEY from env)"))
    <*> (commands <|> hiddenCommands)
    <**> infoOption (T.unpack fullVersionDescription) (long "version" <> short 'V' <> help "show version text")
    <**> helper

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
          )

hiddenCommands :: Parser Command
hiddenCommands =
  hsubparser
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
          "container"
          ( info
              (ContainerCommand <$> containerOpts)
              (progDesc "Run in Container Scan mode")
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
    <*> optional (strOption (long "branch" <> help "this repository's current branch (default: current VCS branch)"))
    <*> metadataOpts
    <*> many filterOpt
    <*> baseDirArg

filterOpt :: Parser BuildTargetFilter
filterOpt = option (eitherReader parseFilter) (long "filter" <> help "Analysis-Target filters (default: none)" <> metavar "ANALYSIS-TARGET")
  where
    parseFilter :: String -> Either String BuildTargetFilter
    parseFilter = first errorBundlePretty . runParser filterParser "stdin" . T.pack

metadataOpts :: Parser ProjectMetadata
metadataOpts =
  ProjectMetadata
    <$> optional (strOption (long "title" <> help "the title of the FOSSA project. (default: the project name)"))
    <*> optional (strOption (long "project-url" <> help "this repository's home page"))
    <*> optional (strOption (long "jira-project-key" <> help "this repository's JIRA project key"))
    <*> optional (strOption (long "link" <> help "a link to attach to the current build"))
    <*> optional (strOption (long "team" <> help "this repository's team inside your organization"))
    <*> optional (strOption (long "policy" <> help "the policy to assign to this project in FOSSA"))

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
vpsOpts = VPSOptions <$> skipIprScanOpt <*> licenseOnlyScanOpt <*> fileFilterOpt <*> vpsCommands
  where
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
    <*> flagOpt WriteEnabled (long "write" <> help "Enable writing NOTICE files to disk instead of logging them.")

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
              (progDesc "Generate notice files for AOSP make and blueprint targets")    
          )
    )

containerOpts :: Parser ContainerOptions
containerOpts = ContainerOptions <$> containerCommands

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

containerAnalyzeOpts :: Parser ContainerAnalyzeOptions
containerAnalyzeOpts =
  ContainerAnalyzeOptions
    <$> switch (long "output" <> short 'o' <> help "Output results to stdout instead of uploading to fossa")
    <*> metadataOpts
    <*> imageTextArg

containerTestOpts :: Parser ContainerTestOptions
containerTestOpts =
  ContainerTestOptions
    <$> option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)" <> value 600)
    <*> flag ContainerTest.TestOutputPretty ContainerTest.TestOutputJson (long "json" <> help "Output issues as json")
    <*> imageTextArg

compatibilityOpts :: Parser [Argument]
compatibilityOpts =
    many argumentParser

data CmdOptions = CmdOptions
  { optDebug :: Bool,
    optBaseUrl :: URI,
    optProjectName :: Maybe Text,
    optProjectRevision :: Maybe Text,
    optAPIKey :: Maybe Text,
    optCommand :: Command
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
  { vpsReportJsonOutput :: Bool,
    vpsReportTimeout :: Int,
    vpsReportType :: VPSReport.ReportType,
    vpsReportBaseDir :: FilePath
  }

data ReportOptions = ReportOptions
  { reportJsonOutput :: Bool,
    reportTimeout :: Int,
    reportType :: Report.ReportType,
    reportBaseDir :: FilePath
  }

data AnalyzeOptions = AnalyzeOptions
  { analyzeOutput :: Bool,
    analyzeUnpackArchives :: Flag UnpackArchives,
    analyzeBranch :: Maybe Text,
    analyzeMetadata :: ProjectMetadata,
    analyzeBuildTargetFilters :: [BuildTargetFilter],
    analyzeBaseDir :: FilePath
  }

data TestOptions = TestOptions
  { testTimeout :: Int,
    testOutputType :: Test.TestOutputType,
    testBaseDir :: FilePath
  }

data VPSOptions = VPSOptions
  { skipIprScan :: Flag SkipIPRScan,
    licenseOnlyScan :: Flag LicenseOnlyScan,
    vpsFileFilter :: FilterExpressions,
    vpsCommand :: VPSCommand
  }

data VPSAnalyzeOptions = VPSAnalyzeOptions
  { vpsAnalyzeBaseDir :: FilePath,
    vpsAnalyzeMeta :: ProjectMetadata
  }

data VPSAOSPNoticeOptions = VPSAOSPNoticeOptions
  { vpsAOSPNoticeBaseDir :: FilePath,
    vpsAOSPNoticeWriteEnabled :: Flag WriteEnabled
  }

data VPSTestOptions = VPSTestOptions
  { vpsTestTimeout :: Int,
    vpsTestOutputType :: VPSTest.TestOutputType,
    vpsTestBaseDir :: FilePath
  }

newtype ContainerOptions = ContainerOptions
  { containerCommand :: ContainerCommand }

data ContainerCommand
  = ContainerAnalyze ContainerAnalyzeOptions
  | ContainerTest ContainerTestOptions

data ContainerAnalyzeOptions = ContainerAnalyzeOptions
  { containerAnalyzeOutput :: Bool,
    containerMetadata :: ProjectMetadata,
    containerAnalyzeImage :: ImageText
  }

data ContainerTestOptions = ContainerTestOptions
  { containerTestTimeout :: Int,
    containerTestOutputType:: ContainerTest.TestOutputType,
    containerTestImage :: ImageText
  }