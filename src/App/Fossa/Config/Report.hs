{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Report (
  ReportConfig (..),
  ReportCliOptions,
  ReportOutputFormat (..),
  ReportType (..),
  mkSubCommand,
  -- Exported for testing
  parseReportOutputFormat,
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
import Effect.Logger (Logger, Severity (..))
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
  progDescDoc,
  strOption,
  switch,
 )
import Prettyprinter (Doc, comma, hardline, pretty, punctuate, softline, viaShow)
import Prettyprinter.Render.Terminal (AnsiStyle)

data ReportType = Attribution deriving (Eq, Ord, Enum, Bounded, Generic)

instance ToJSON ReportType where
  toEncoding = genericToEncoding defaultOptions

instance Show ReportType where
  show Attribution = "attribution"

data ReportOutputFormat
  = ReportCSV
  | -- Core will return the cyclonedx report with license data encoded in base64.
    -- This is specified in the bom schema: https://github.com/CycloneDX/specification/blob/1.4/schema/bom-1.4.schema.json#L529
    ReportCycloneDXJSON
  | ReportCycloneDXXML
  | ReportHTML
  | ReportJson
  | ReportMarkdown
  | ReportPlainText
  | ReportSpdx
  | ReportSpdxJSON
  deriving (Eq, Ord, Enum, Bounded, Generic)

parseReportOutputFormat :: String -> Maybe ReportOutputFormat
parseReportOutputFormat s | s == show ReportJson = Just ReportJson
parseReportOutputFormat s | s == show ReportSpdx = Just ReportSpdx
parseReportOutputFormat s | s == show ReportMarkdown = Just ReportMarkdown
parseReportOutputFormat s | s == show ReportPlainText = Just ReportPlainText
parseReportOutputFormat s | s == show ReportSpdxJSON = Just ReportSpdxJSON
parseReportOutputFormat s | s == show ReportCycloneDXJSON = Just ReportCycloneDXJSON
parseReportOutputFormat s | s == show ReportCycloneDXXML = Just ReportCycloneDXXML
parseReportOutputFormat s | s == show ReportHTML = Just ReportHTML
parseReportOutputFormat s | s == show ReportCSV = Just ReportCSV
parseReportOutputFormat _ = Nothing

instance ToText ReportOutputFormat where
  toText = toText . show

instance Show ReportOutputFormat where
  show ReportJson = "json"
  show ReportMarkdown = "markdown"
  show ReportSpdx = "spdx"
  show ReportPlainText = "text"
  show ReportSpdxJSON = "spdx-json"
  show ReportCycloneDXJSON = "cyclonedx-json"
  show ReportCycloneDXXML = "cyclonedx-xml"
  show ReportHTML = "html"
  show ReportCSV = "csv"

reportOutputFormatList :: String
reportOutputFormatList = intercalate ", " $ map show allFormats
  where
    allFormats :: [ReportOutputFormat]
    allFormats = enumFromTo minBound maxBound

instance ToJSON ReportOutputFormat where
  toEncoding = genericToEncoding defaultOptions

reportInfo :: InfoMod a
reportInfo = progDescDoc (Just desc)
  where
    allReports :: [ReportType]
    allReports = enumFromTo minBound maxBound

    desc :: Doc AnsiStyle
    desc =
      "Access various reports from FOSSA and print to stdout."
        <> softline
        <> "Currently available reports: ("
        <> mconcat (punctuate comma (map viaShow allReports))
        <> ")"
        <> hardline
        <> "Examples: "
        <> hardline
        <> "fossa report --format html attribution"

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
  , Has Logger sig m
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
