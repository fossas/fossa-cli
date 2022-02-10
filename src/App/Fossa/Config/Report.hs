{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Report (
  ReportConfig (..),
  ReportCliOptions,
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
import App.Fossa.Config.ConfigFile (ConfigFile, resolveConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Subcommand (EffStack, GetSeverity (getSeverity), SubCommand (SubCommand))
import App.Types (BaseDir, OverrideProject (OverrideProject), ProjectRevision)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Timeout (Duration (Seconds))
import Data.List (intercalate)
import Effect.Exec (Exec)
import Effect.Logger (Logger, Severity (..))
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts)
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
  switch,
 )
import Path.IO (getCurrentDir)

data ReportType = Attribution deriving (Eq, Ord, Enum, Bounded)

instance Show ReportType where
  show Attribution = "attribution"

-- TODO: Add support for text-format reports
data ReportOutputFormat
  = ReportJson
  -- ReportPretty
  deriving (Eq, Ord, Show)

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
    <*> switch (long "json" <> help "Output the report in JSON format (Currently required).")
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
  , cliReportTimeout :: Maybe Int
  , cliReportType :: ReportType
  , cliReportBaseDir :: FilePath
  }
  deriving (Eq, Ord, Show)

instance GetSeverity ReportCliOptions where
  getSeverity ReportCliOptions{..} = if (optDebug commons) then SevDebug else SevInfo

loadConfig ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  ReportCliOptions ->
  m (Maybe ConfigFile)
loadConfig ReportCliOptions{commons = CommonOpts{optConfig}} = do
  configRelBase <- sendIO getCurrentDir
  resolveConfigFile configRelBase optConfig

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
      outputformat = validateOutputFormat cliReportJsonOutput
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

validateOutputFormat :: Has Diagnostics sig m => Bool -> m ReportOutputFormat
validateOutputFormat doJson =
  if doJson
    then pure ReportJson
    else fatalText "Plaintext reports are not available for this report."

data ReportConfig = ReportConfig
  { apiOpts :: ApiOpts
  , baseDir :: BaseDir
  , outputFormat :: ReportOutputFormat
  , timeoutDuration :: Duration
  , reportType :: ReportType
  , revision :: ProjectRevision
  }
  deriving (Eq, Ord, Show)
