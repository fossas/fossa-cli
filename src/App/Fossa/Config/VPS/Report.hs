{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.VPS.Report (
  ReportConfig (..),
  ReportOpts (..),
  ReportOutputFormat (..),
  ReportType (..),
  mergeOpts,
  subcommand,
) where

import App.Fossa.Config.Common (
  CacheAction (ReadOnly),
  CommonOpts (optProjectName, optProjectRevision),
  baseDirArg,
  collectApiOpts,
  collectBaseDir,
  collectRevisionData',
  commonOpts,
  defaultTimeoutDuration,
 )
import App.Fossa.Config.ConfigFile (ConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Types (
  BaseDir,
  OverrideProject (OverrideProject),
  ProjectRevision,
 )
import Control.Effect.Diagnostics (Diagnostics, Has, fatalText)
import Control.Effect.Lift (Lift)
import Control.Timeout (Duration (Seconds))
import Effect.Exec (Exec)
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts)
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  argument,
  auto,
  command,
  help,
  info,
  long,
  maybeReader,
  metavar,
  option,
  optional,
  progDesc,
  switch,
 )

data ReportOpts = ReportOpts
  { reportCommons :: CommonOpts
  , vpsReportJsonOutput :: Bool
  , vpsReportTimeout :: Maybe Int
  , vpsReportType :: ReportType
  , vpsReportBaseDir :: FilePath
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

data ReportOutputFormat
  = ReportJson
  -- ReportPretty
  deriving (Eq, Ord, Show)

data ReportType = Attribution deriving (Eq, Ord)

instance Show ReportType where
  show Attribution = "attribution"

mergeOpts ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  ReportOpts ->
  m ReportConfig
mergeOpts cfgfile envvars ReportOpts{..} = do
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

subcommand :: (ReportOpts -> a) -> Mod CommandFields a
subcommand f =
  command
    "report"
    ( info
        (f <$> cliParser)
        (progDesc "Access various reports from FOSSA and print to stdout")
    )

cliParser :: Parser ReportOpts
cliParser =
  ReportOpts
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
