{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.VPS.Analyze (
  AnalyzeConfig (..),
  AnalyzeOpts (..),
  FollowSymlinks (..),
  LicenseOnlyScan (..),
  SkipIPRScan (..),
  mergeOpts,
  subcommand,
) where

import App.Fossa.Config.Common (
  CacheAction (WriteOnly),
  CommonOpts (optDebug, optProjectName, optProjectRevision),
  baseDirArg,
  collectApiOpts,
  collectBaseDir,
  collectRevisionData',
  commonOpts,
  metadataOpts,
 )
import App.Fossa.Config.ConfigFile (ConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Config.VPS.Common (collectProjectMetadata)
import App.Fossa.VPS.Types (FilterExpressions (FilterExpressions))
import App.OptionExtensions (jsonOption)
import App.Types (
  BaseDir,
  OverrideProject (OverrideProject),
  ProjectMetadata,
  ProjectRevision,
 )
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Effect.Lift (Lift)
import Data.Flag (Flag, flagOpt)
import Effect.Exec (Exec)
import Effect.Logger (Severity (SevDebug, SevInfo))
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (ApiOpts)
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  command,
  help,
  info,
  long,
  metavar,
  progDesc,
  short,
  value,
 )

data FollowSymlinks = FollowSymlinks
data LicenseOnlyScan = LicenseOnlyScan
data SkipIPRScan = SkipIPRScan

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

data AnalyzeOpts = AnalyzeOpts
  { analyzeCliCommons :: CommonOpts
  , analyzeCliFollowSymlinks :: Flag FollowSymlinks
  , analyzeCliSkipIprScan :: Flag SkipIPRScan
  , analyzeCliLicenseOnlyScan :: Flag LicenseOnlyScan
  , analyzeCliFileFilter :: FilterExpressions
  , analyzeCliBaseDir :: FilePath
  , analyzeCliMetadata :: ProjectMetadata
  }
  deriving (Eq, Ord, Show)

subcommand :: (AnalyzeOpts -> a) -> Mod CommandFields a
subcommand f =
  command
    "analyze"
    ( info (f <$> cliParser) $
        progDesc "Scan for projects and their vendored dependencies"
    )

mergeOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  AnalyzeOpts ->
  m AnalyzeConfig
mergeOpts cfgfile envvars AnalyzeOpts{..} = do
  let metadata = collectProjectMetadata cfgfile analyzeCliMetadata
      severity = if (optDebug analyzeCliCommons) then SevDebug else SevInfo
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

cliParser :: Parser AnalyzeOpts
cliParser =
  AnalyzeOpts
    <$> commonOpts
    <*> flagOpt FollowSymlinks (long "follow" <> help "If specified, follows symbolic links (does not protect against cyclic links)")
    <*> flagOpt SkipIPRScan (long "skip-ipr-scan" <> help "If specified, the scan directory will not be scanned for intellectual property rights information")
    <*> flagOpt LicenseOnlyScan (long "license-only" <> help "If specified, the scan directory will not be scanned for vendored dependencies")
    <*> (FilterExpressions <$> jsonOption (long "ignore-file-regex" <> short 'i' <> metavar "REGEXPS" <> help "JSON encoded array of regular expressions used to filter scanned paths" <> value []))
    <*> baseDirArg
    <*> metadataOpts
