{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Container (
  mkSubCommand,
  ImageText (..),
  OutputFormat (..),
  ContainerCommand,
  ContainerScanConfig (..),
  ContainerAnalyzeConfig (..),
  ContainerTestConfig (..),
  ContainerDumpScanConfig (..),
  ContainerParseFileConfig (..),
) where

import App.Fossa.Config.Common (
  CommonOpts (..),
  ScanDestination (..),
  collectAPIMetadata,
  collectApiOpts,
  collectRevisionOverride,
  commonOpts,
  defaultTimeoutDuration,
  metadataOpts,
  validateApiKey,
 )
import App.Fossa.Config.ConfigFile (
  ConfigFile,
  resolveConfigFile,
 )
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Subcommand (EffStack, GetSeverity (getSeverity), SubCommand (SubCommand))
import App.Types (
  OverrideProject (OverrideProject),
  ProjectMetadata,
 )
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Timeout (Duration (Seconds))
import Data.Flag (Flag, flagOpt, fromFlag)
import Data.Text (Text)
import Effect.Logger (Logger, Severity (SevDebug, SevInfo))
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts (ApiOpts))
import Options.Applicative (
  CommandFields,
  InfoMod,
  Mod,
  Parser,
  argument,
  auto,
  command,
  flag,
  help,
  hsubparser,
  info,
  internal,
  long,
  metavar,
  option,
  optional,
  progDesc,
  short,
  str,
  strOption,
 )
import Path (Abs, File, Path)
import Path.IO (getCurrentDir, resolveFile)

data OutputFormat
  = TestOutputPretty
  | TestOutputJson
  deriving (Eq, Ord, Show)

data NoUpload = NoUpload

newtype ImageText = ImageText
  { unImageText :: Text
  }
  deriving (Eq, Ord, Show)

containerCmdInfo :: InfoMod a
containerCmdInfo = progDesc "Run in container-scanning mode"

mkSubCommand :: (ContainerScanConfig -> EffStack ()) -> SubCommand ContainerCommand ContainerScanConfig
mkSubCommand = SubCommand "container" containerCmdInfo parser loadConfig mergeOpts

mergeOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  ContainerCommand ->
  m ContainerScanConfig
mergeOpts cfgfile envvars = \case
  ContainerAnalyze opts -> AnalyzeCfg <$> mergeAnalyzeOpts cfgfile envvars opts
  ContainerTest opts -> TestCfg <$> mergeTestOpts cfgfile envvars opts
  ContainerParseFile fp -> ParseCfg <$> mergeParseFileOpts cfgfile envvars fp
  ContainerDumpScan opts -> DumpCfg <$> mergeDumpOpts cfgfile envvars opts

mergeAnalyzeOpts ::
  Has Diagnostics sig m =>
  Maybe ConfigFile ->
  EnvVars ->
  ContainerAnalyzeOptions ->
  m ContainerAnalyzeConfig
mergeAnalyzeOpts cfgfile envvars cliOpts@ContainerAnalyzeOptions{..} = do
  let scanDest = collectScanDestination cfgfile envvars cliOpts
      imageLoc = containerAnalyzeImage
      revOverride =
        collectRevisionOverride cfgfile $
          OverrideProject
            (optProjectName analyzeCommons)
            (optProjectRevision analyzeCommons)
            (containerBranch)
  ContainerAnalyzeConfig
    <$> scanDest
    <*> pure revOverride
    <*> pure imageLoc

collectScanDestination ::
  Has Diagnostics sig m =>
  Maybe ConfigFile ->
  EnvVars ->
  ContainerAnalyzeOptions ->
  m ScanDestination
collectScanDestination maybeCfgFile envvars ContainerAnalyzeOptions{..} =
  if fromFlag NoUpload containerNoUpload
    then pure OutputStdout
    else do
      apiKey <- validateApiKey maybeCfgFile envvars analyzeCommons
      let baseuri = optBaseUrl analyzeCommons
          apiOpts = ApiOpts baseuri apiKey
          metaMerged = collectAPIMetadata maybeCfgFile containerMetadata
      pure $ UploadScan apiOpts metaMerged

mergeTestOpts ::
  Has Diagnostics sig m =>
  Maybe ConfigFile ->
  EnvVars ->
  ContainerTestOptions ->
  m ContainerTestConfig
mergeTestOpts cfgfile envvars ContainerTestOptions{..} = do
  let apiopts = collectApiOpts cfgfile envvars testCommons
      timeout = maybe defaultTimeoutDuration Seconds containerTestTimeout
      revOverride =
        collectRevisionOverride cfgfile $
          OverrideProject (optProjectName testCommons) (optProjectRevision testCommons) Nothing
  ContainerTestConfig
    <$> apiopts
    <*> pure timeout
    <*> pure containerTestOutputType
    <*> pure containerTestImage
    <*> pure revOverride

mergeParseFileOpts :: Has (Lift IO) sig m => Maybe ConfigFile -> EnvVars -> FilePath -> m ContainerParseFileConfig
mergeParseFileOpts _ _ fp = do
  curdir <- sendIO getCurrentDir
  path <- sendIO $ resolveFile curdir fp
  pure $ ContainerParseFileConfig path

mergeDumpOpts ::
  Has (Lift IO) sig m =>
  Maybe ConfigFile ->
  EnvVars ->
  ContainerDumpScanOptions ->
  m ContainerDumpScanConfig
mergeDumpOpts _ _ ContainerDumpScanOptions{..} = do
  curdir <- sendIO getCurrentDir
  maybeOut <- case dumpScanOutputFile of
    Nothing -> pure Nothing
    Just fp -> sendIO $ Just <$> resolveFile curdir fp
  pure $ ContainerDumpScanConfig maybeOut dumpScanImage

loadConfig ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  ContainerCommand ->
  m (Maybe ConfigFile)
loadConfig = \case
  ContainerParseFile _ -> pure Nothing
  ContainerDumpScan _ -> pure Nothing
  -- Only parse config file if we're running analyze or test
  cmd -> do
    curdir <- sendIO getCurrentDir
    resolveConfigFile curdir $ getCfgFilePath cmd

getCfgFilePath :: ContainerCommand -> Maybe FilePath
getCfgFilePath = \case
  ContainerAnalyze opts -> optConfig $ analyzeCommons opts
  ContainerTest opts -> optConfig $ testCommons opts
  -- We only use the config file for analyze and test
  _ -> Nothing

data ContainerCommand
  = ContainerAnalyze ContainerAnalyzeOptions
  | ContainerTest ContainerTestOptions
  | ContainerParseFile FilePath
  | ContainerDumpScan ContainerDumpScanOptions

data ContainerAnalyzeOptions = ContainerAnalyzeOptions
  { analyzeCommons :: CommonOpts
  , containerNoUpload :: Flag NoUpload
  , containerBranch :: Maybe Text
  , containerMetadata :: ProjectMetadata
  , containerAnalyzeImage :: ImageText
  }

data ContainerTestOptions = ContainerTestOptions
  { testCommons :: CommonOpts
  , containerTestTimeout :: Maybe Int
  , containerTestOutputType :: OutputFormat
  , containerTestImage :: ImageText
  }

data ContainerDumpScanOptions = ContainerDumpScanOptions
  { dumpScanOutputFile :: Maybe FilePath
  , dumpScanImage :: ImageText
  }

data ContainerScanConfig
  = AnalyzeCfg ContainerAnalyzeConfig
  | TestCfg ContainerTestConfig
  | DumpCfg ContainerDumpScanConfig
  | ParseCfg ContainerParseFileConfig

instance GetSeverity ContainerCommand where
  getSeverity = \case
    ContainerAnalyze (ContainerAnalyzeOptions{analyzeCommons = CommonOpts{optDebug}}) -> fromBool optDebug
    ContainerTest (ContainerTestOptions{testCommons = CommonOpts{optDebug}}) -> fromBool optDebug
    ContainerParseFile _ -> SevInfo
    ContainerDumpScan _ -> SevInfo
    where
      fromBool b = if b then SevDebug else SevInfo

data ContainerAnalyzeConfig = ContainerAnalyzeConfig
  { scanDestination :: ScanDestination
  , revisionOverride :: OverrideProject
  , imageLocator :: ImageText
  }
  deriving (Eq, Ord, Show)

data ContainerTestConfig = ContainerTestConfig
  { apiOpts :: ApiOpts
  , timeoutDuration :: Duration
  , outputFormat :: OutputFormat
  , testImageLocator :: ImageText
  , testRevisionOverride :: OverrideProject
  }
  deriving (Eq, Ord, Show)

data ContainerDumpScanConfig = ContainerDumpScanConfig
  { outputFile :: Maybe (Path Abs File)
  , dumpImageLocator :: ImageText
  }
  deriving (Eq, Ord, Show)

newtype ContainerParseFileConfig = ContainerParseFileConfig
  { sourceFile :: Path Abs File
  }
  deriving (Eq, Ord, Show)

parser :: Parser ContainerCommand
parser =
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
        <> hiddenContainerCommands
    )

hiddenContainerCommands :: Mod CommandFields ContainerCommand
hiddenContainerCommands =
  internal
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

containerAnalyzeOpts :: Parser ContainerAnalyzeOptions
containerAnalyzeOpts =
  ContainerAnalyzeOptions
    <$> commonOpts
    <*> flagOpt
      NoUpload
      ( long "output"
          <> short 'o'
          <> help "Output results to stdout instead of uploading to fossa"
      )
    <*> optional
      ( strOption
          ( long "branch"
              <> short 'b'
              <> help "this repository's current branch (default: current VCS branch)"
          )
      )
    <*> metadataOpts
    <*> imageTextArg

containerTestOpts :: Parser ContainerTestOptions
containerTestOpts =
  ContainerTestOptions
    <$> commonOpts
    <*> optional (option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)"))
    <*> flag TestOutputPretty TestOutputJson (long "json" <> help "Output issues as json")
    <*> imageTextArg

containerParseFileOptions :: Parser FilePath
containerParseFileOptions = argument str (metavar "FILE" <> help "File to parse")

containerDumpScanOptions :: Parser ContainerDumpScanOptions
containerDumpScanOptions =
  ContainerDumpScanOptions
    <$> optional
      ( strOption
          ( short 'o'
              <> long "output-file"
              <> help "File to write the scan data (omit for stdout)"
          )
      )
    <*> imageTextArg

imageTextArg :: Parser ImageText
imageTextArg = ImageText <$> argument str (metavar "IMAGE" <> help "The image to scan")
