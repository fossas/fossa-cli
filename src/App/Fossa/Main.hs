{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module App.Fossa.Main
  ( appMain,
  )
where

import App.Fossa.Analyze (ScanDestination (..), analyzeMain)
import App.Fossa.ListTargets (listTargetsMain)
import App.Fossa.Report (ReportType (..), reportMain)
import App.Fossa.Test (TestOutputType (..), testMain)
import App.Fossa.VPS.NinjaGraph
import App.Fossa.VPS.Scan
    ( scanMain,
      SkipIPRScan(..) )
import App.Fossa.VPS.Types ( FilterExpressions(..) )
import App.OptionExtensions
import App.Types
import App.Util (validateDir)
import Control.Monad (when, unless)
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Text (Text)
import qualified Data.Text as T
import Discovery.Filters (BuildTargetFilter(..), filterParser)
import Effect.Logger
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
          analyzeMain baseDir logSeverity (UploadScan $ UploadInfo optBaseUrl key analyzeMetadata) analyzeOverride analyzeUnpackArchives analyzeBuildTargetFilters

    TestCommand TestOptions {..} -> do
      baseDir <- validateDir testBaseDir
      key <- requireKey maybeApiKey
      testMain optBaseUrl baseDir key logSeverity testTimeout testOutputType override

    InitCommand ->
      withLogger logSeverity $ logWarn "This command has been deprecated and is no longer needed.  It has no effect and may be safely removed."

    ReportCommand ReportOptions {..} -> do
      unless reportJsonOutput $ die "report command currently only supports JSON output.  Please try `fossa report --json REPORT_NAME`"
      baseDir <- validateDir reportBaseDir
      key <- requireKey maybeApiKey
      reportMain optBaseUrl baseDir key logSeverity reportTimeout reportType override
    
    ListTargetsCommand dir -> do
      baseDir <- validateDir dir
      listTargetsMain baseDir

    VPSCommand VPSOptions {..} -> do
      when (SysInfo.os == windowsOsName) $ die "VPS functionality is not supported on Windows"
      apikey <- requireKey maybeApiKey
      case vpsCommand of
        VPSAnalyzeCommand VPSAnalyzeOptions {..} -> do
          baseDir <- validateDir vpsAnalyzeBaseDir
          let uploadInfo = UploadInfo optBaseUrl apikey vpsAnalyzeMeta
          scanMain baseDir logSeverity uploadInfo override vpsFileFilter (SkipIPRScan skipIprScan)
        NinjaGraphCommand ninjaGraphOptions -> do
          ninjaGraphMain optBaseUrl apikey logSeverity override ninjaGraphOptions
          

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
    )

analyzeOpts :: Parser AnalyzeOptions
analyzeOpts =
  AnalyzeOptions
    <$> switch (long "output" <> short 'o' <> help "Output results to stdout instead of uploading to fossa")
    <*> switch (long "unpack-archives" <> help "Recursively unpack and analyze discovered archives")
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

reportCmd :: Parser ReportType
reportCmd =
  hsubparser $
    command "attribution" (info (pure AttributionReport) $ progDesc "Generate attribution report" )

testOpts :: Parser TestOptions
testOpts =
  TestOptions
    <$> option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)" <> value 600)
    <*> flag TestOutputPretty TestOutputJson (long "json" <> help "Output issues as json")
    <*> baseDirArg

vpsOpts :: Parser VPSOptions
vpsOpts = VPSOptions <$> skipIprScanOpt <*> fileFilterOpt <*> vpsCommands
  where
    skipIprScanOpt = switch (long "skip-ipr-scan" <> help "If specified, the scan directory will not be scanned for intellectual property rights information")
    fileFilterOpt = FilterExpressions <$> jsonOption (long "ignore-file-regex" <> short 'i' <> metavar "REGEXPS" <> help "JSON encoded array of regular expressions used to filter scanned paths" <> value [])

vpsAnalyzeOpts :: Parser VPSAnalyzeOptions
vpsAnalyzeOpts = VPSAnalyzeOptions <$> baseDirArg <*> metadataOpts

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
    (
      command "analyze"
        (info (VPSAnalyzeCommand <$> vpsAnalyzeOpts) $
          progDesc "Scan for projects and their vendored dependencies"
        )
    <>
      command "ninja-graph"
        (info (NinjaGraphCommand <$> ninjaGraphOpts) $
          progDesc "Get a dependency graph for a ninja build"
        )
    )


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
  | InitCommand
  | ListTargetsCommand FilePath

data VPSCommand
  = VPSAnalyzeCommand VPSAnalyzeOptions
  | NinjaGraphCommand NinjaGraphCLIOptions

data ReportOptions = ReportOptions
  { reportJsonOutput :: Bool,
    reportTimeout :: Int,
    reportType :: ReportType,
    reportBaseDir :: FilePath
  }

data AnalyzeOptions = AnalyzeOptions
  { analyzeOutput :: Bool,
    analyzeUnpackArchives :: Bool,
    analyzeBranch :: Maybe Text,
    analyzeMetadata :: ProjectMetadata,
    analyzeBuildTargetFilters :: [BuildTargetFilter],
    analyzeBaseDir :: FilePath
  }

data TestOptions = TestOptions
  { testTimeout :: Int,
    testOutputType :: TestOutputType,
    testBaseDir :: FilePath
  }

data VPSOptions = VPSOptions
  { skipIprScan :: Bool,
    vpsFileFilter :: FilterExpressions,
    vpsCommand :: VPSCommand
  }

data VPSAnalyzeOptions = VPSAnalyzeOptions
  { vpsAnalyzeBaseDir :: FilePath,
    vpsAnalyzeMeta :: ProjectMetadata
  }
