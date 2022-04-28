{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Report (
  ReportConfig (..),
  ReportCliOptions,
  ReportOutputFormat (..),
  ReportType (..),
  mkSubCommand,
) where

import App.Fossa.Config.Common (
  CacheAction (ReadOnly),
  CommonOpts (..),
  baseDirArg,
  collectApiOpts,
  collectBaseDir,
  collectRevisionData',
  commonOpts,
  defaultTimeoutDuration,
 )
import App.Fossa.Config.ConfigFile (ConfigFile, resolveLocalConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Subcommand (EffStack, GetCommonOpts (getCommonOpts), GetSeverity (getSeverity), SubCommand (SubCommand))
import App.Types (BaseDir, OverrideProject (OverrideProject), ProjectRevision)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic (renderDiagnostic), fatal, fromMaybe)
import Control.Effect.Lift (Has, Lift)
import Control.Timeout (Duration (Seconds))
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.List (intercalate)
import Data.String.Conversion (ToText, toText)
import Effect.Exec (Exec)
import Effect.Logger (Logger, Severity (..), pretty)
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts)
import GHC.Generics (Generic)
import Options.Applicative (
  InfoMod,
  Parser,
  argument,
  auto,
  help,
  long,
  maybeReader,
  metavar,
  option,
  optional,
  progDesc,
  strOption,
  switch,
 )

data ReportType = Attribution deriving (Eq, Ord, Enum, Bounded, Generic)

instance ToJSON ReportType where
  toEncoding = genericToEncoding defaultOptions

instance Show ReportType where
  show Attribution = "attribution"

data ReportOutputFormat
  = ReportJson
  | ReportMarkdown
  | ReportSpdx
  | ReportPlainText
  deriving (Eq, Ord, Enum, Bounded, Generic)

parseReportOutputFormat :: String -> Maybe ReportOutputFormat
parseReportOutputFormat s | s == show ReportJson = Just ReportJson
parseReportOutputFormat s | s == show ReportSpdx = Just ReportSpdx
parseReportOutputFormat s | s == show ReportMarkdown = Just ReportMarkdown
parseReportOutputFormat s | s == show ReportPlainText = Just ReportPlainText
parseReportOutputFormat _ = Nothing

instance ToText ReportOutputFormat where
  toText = toText . show

instance Show ReportOutputFormat where
  show ReportJson = "json"
  show ReportMarkdown = "markdown"
  show ReportSpdx = "spdx"
  show ReportPlainText = "text"

reportOutputFormatList :: String
reportOutputFormatList = intercalate ", " $ map show allFormats
  where
    allFormats :: [ReportOutputFormat]
    allFormats = enumFromTo minBound maxBound

instance ToJSON ReportOutputFormat where
  toEncoding = genericToEncoding defaultOptions

reportInfo :: InfoMod a
reportInfo = progDesc desc
  where
    allReports :: [ReportType]
    allReports = enumFromTo minBound maxBound

    desc =
      "Access various reports from FOSSA and print to stdout.  Currently available reports: ("
        <> intercalate ", " (map show allReports)
        <> ")"

mkSubCommand :: (ReportConfig -> EffStack ()) -> SubCommand ReportCliOptions ReportConfig
mkSubCommand = SubCommand "report" reportInfo parser loadConfig mergeOpts

parser :: Parser ReportCliOptions
parser =
  ReportCliOptions
    <$> commonOpts
    <*> switch (long "json" <> help "Output the report in JSON format. Equivalent to '--format json', and overrides --format. Deprecated: prefer --format")
    <*> optional (strOption (long "format" <> help ("Output the report in the specified format. Currently available formats: (" <> reportOutputFormatList <> ")")))
    <*> optional (option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)"))
    <*> reportTypeArg
    <*> baseDirArg

reportTypeArg :: Parser ReportType
reportTypeArg = argument (maybeReader parseType) (metavar "REPORT" <> help "The report type to fetch from the server.")
  where
    parseType :: String -> Maybe ReportType
    parseType = \case
      "attribution" -> Just Attribution
      _ -> Nothing

data ReportCliOptions = ReportCliOptions
  { commons :: CommonOpts
  , cliReportJsonOutput :: Bool
  , cliReportOutputFormat :: Maybe String
  , cliReportTimeout :: Maybe Int
  , cliReportType :: ReportType
  , cliReportBaseDir :: FilePath
  }
  deriving (Eq, Ord, Show)

instance GetSeverity ReportCliOptions where
  getSeverity ReportCliOptions{..} = if (optDebug commons) then SevDebug else SevInfo

instance GetCommonOpts ReportCliOptions where
  getCommonOpts ReportCliOptions{..} = Just commons

loadConfig ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  ReportCliOptions ->
  m (Maybe ConfigFile)
loadConfig = resolveLocalConfigFile . optConfig . commons

mergeOpts ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Exec sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  ReportCliOptions ->
  m ReportConfig
mergeOpts cfgfile envvars ReportCliOptions{..} = do
  let apiOpts = collectApiOpts cfgfile envvars commons
      basedir = collectBaseDir cliReportBaseDir
      outputformat = validateOutputFormat cliReportJsonOutput cliReportOutputFormat
      timeoutduration = maybe defaultTimeoutDuration Seconds cliReportTimeout
      revision =
        collectRevisionData' basedir cfgfile ReadOnly $
          OverrideProject (optProjectName commons) (optProjectRevision commons) Nothing
  ReportConfig
    <$> apiOpts
    <*> basedir
    <*> outputformat
    <*> pure timeoutduration
    <*> pure cliReportType
    <*> revision

data NoFormatProvided = NoFormatProvided
instance ToDiagnostic NoFormatProvided where
  renderDiagnostic NoFormatProvided =
    pretty $
      "Provide a format option via '--format' to render this report. Supported formats: "
        <> (toText reportOutputFormatList)

newtype InvalidReportFormat = InvalidReportFormat String
instance ToDiagnostic InvalidReportFormat where
  renderDiagnostic (InvalidReportFormat fmt) =
    pretty $
      "Report format "
        <> toText fmt
        <> " is not supported. Supported formats: "
        <> (toText reportOutputFormatList)

validateOutputFormat :: Has Diagnostics sig m => Bool -> Maybe String -> m ReportOutputFormat
validateOutputFormat True _ = pure ReportJson
validateOutputFormat False Nothing = fatal NoFormatProvided
validateOutputFormat False (Just format) = fromMaybe (InvalidReportFormat format) $ parseReportOutputFormat format

data ReportConfig = ReportConfig
  { apiOpts :: ApiOpts
  , baseDir :: BaseDir
  , outputFormat :: ReportOutputFormat
  , timeoutDuration :: Duration
  , reportType :: ReportType
  , revision :: ProjectRevision
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ReportConfig where
  toEncoding = genericToEncoding defaultOptions
