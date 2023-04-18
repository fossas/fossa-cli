{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Container.Analyze (
  NoUpload (..),
  JsonOutput (..),
  ContainerAnalyzeConfig (..),
  ContainerAnalyzeOptions (..),
  cliParser,
  mergeOpts,
  subcommand,
) where

import App.Fossa.Config.Common (
  CommonOpts (CommonOpts, optDebug, optProjectName, optProjectRevision),
  ScanDestination (..),
  collectAPIMetadata,
  collectApiOpts,
  collectConfigFileFilters,
  collectRevisionOverride,
  commonOpts,
  metadataOpts,
 )
import App.Fossa.Config.ConfigFile
import App.Fossa.Config.Container.Common (
  ImageText,
  collectArch,
  collectDockerHost,
  imageTextArg,
 )
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Subcommand (GetSeverity, getSeverity)
import App.Types (
  OverrideProject (OverrideProject),
  ProjectMetadata,
 )
import Control.Effect.Diagnostics (Diagnostics, Has)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding)
import Data.Aeson.Types (ToJSON (toEncoding))
import Data.Flag (Flag, flagOpt, fromFlag)
import Data.Monoid.Extra (isMempty)
import Data.Text (Text)
import Discovery.Filters
import Effect.Logger (Severity (SevDebug, SevInfo))
import GHC.Generics (Generic)
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  command,
  help,
  hidden,
  info,
  long,
  optional,
  progDesc,
  short,
  strOption,
  switch,
 )

data NoUpload = NoUpload
data JsonOutput = JsonOutput deriving (Generic)

data ContainerAnalyzeConfig = ContainerAnalyzeConfig
  { scanDestination :: ScanDestination
  , revisionOverride :: OverrideProject
  , imageLocator :: ImageText
  , jsonOutput :: Flag JsonOutput
  , -- \* For Experimental Scanner
    usesExperimentalScanner :: Bool
  , dockerHost :: Text
  , arch :: Text
  , severity :: Severity
  , onlySystemDeps :: Bool
  , filterSet :: AllFilters
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ContainerAnalyzeConfig where
  toEncoding = genericToEncoding defaultOptions

data ContainerAnalyzeOptions = ContainerAnalyzeOptions
  { analyzeCommons :: CommonOpts
  , containerNoUpload :: Flag NoUpload
  , containerJsonOutput :: Flag JsonOutput
  , containerBranch :: Maybe Text
  , containerMetadata :: ProjectMetadata
  , containerAnalyzeImage :: ImageText
  , containerExperimentalScanner :: Bool
  , containerExperimentalOnlySysDependencies :: Bool
  }

instance GetSeverity ContainerAnalyzeOptions where
  getSeverity ContainerAnalyzeOptions{analyzeCommons = CommonOpts{optDebug}} = if optDebug then SevDebug else SevInfo

subcommand :: (ContainerAnalyzeOptions -> a) -> Mod CommandFields a
subcommand f =
  command
    "analyze"
    ( info (f <$> cliParser) $
        progDesc "Scan an image for vulnerabilities"
    )

cliParser :: Parser ContainerAnalyzeOptions
cliParser =
  ContainerAnalyzeOptions
    <$> commonOpts
    <*> flagOpt
      NoUpload
      ( long "output"
          <> short 'o'
          <> help "Output results to stdout instead of uploading to fossa"
      )
    <*> flagOpt JsonOutput (long "json" <> help "Output project metadata as json to the console. Useful for communicating with the FOSSA API")
    <*> optional
      ( strOption
          ( long "branch"
              <> short 'b'
              <> help "this repository's current branch (default: current VCS branch)"
          )
      )
    <*> metadataOpts
    <*> imageTextArg
    <*> switch (long "experimental-scanner" <> help "Uses experimental fossa native container scanner." <> hidden)
    <*> switch (long "only-system-deps" <> help "Only analyzes system dependencies (e.g. apk, dep, rpm).")

mergeOpts ::
  (Has Diagnostics sig m) =>
  Maybe ConfigFile ->
  EnvVars ->
  ContainerAnalyzeOptions ->
  m ContainerAnalyzeConfig
mergeOpts cfgfile envvars cliOpts@ContainerAnalyzeOptions{..} = do
  let scanDest = collectScanDestination cfgfile envvars cliOpts
      severity = getSeverity cliOpts
      imageLoc = containerAnalyzeImage
      jsonOutput = containerJsonOutput
      arch = collectArch
      onlySystemDeps = containerExperimentalOnlySysDependencies
      scanFilters = collectFilters cfgfile

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
    <*> pure jsonOutput
    <*> pure containerExperimentalScanner
    <*> collectDockerHost envvars
    <*> pure arch
    <*> pure severity
    <*> pure onlySystemDeps
    <*> pure scanFilters

collectScanDestination ::
  (Has Diagnostics sig m) =>
  Maybe ConfigFile ->
  EnvVars ->
  ContainerAnalyzeOptions ->
  m ScanDestination
collectScanDestination maybeCfgFile envvars ContainerAnalyzeOptions{..} =
  if fromFlag NoUpload containerNoUpload
    then pure OutputStdout
    else do
      apiOpts <- collectApiOpts maybeCfgFile envvars analyzeCommons
      let metaMerged = collectAPIMetadata maybeCfgFile containerMetadata
      pure $ UploadScan apiOpts metaMerged

collectFilters :: Maybe ConfigFile -> AllFilters
collectFilters maybeConfig = do
  let cfgFileFilters = maybe mempty collectConfigFileFilters maybeConfig
  if isMempty cfgFileFilters
    then mempty
    else cfgFileFilters
