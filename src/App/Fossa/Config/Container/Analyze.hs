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

import App.Fossa.Config.Analyze (branchHelp)
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
  hidden,
  info,
  long,
  optional,
  short,
  strOption,
  switch,
 )
import Options.Applicative.Builder (helpDoc, progDescDoc)
import Style (applyFossaStyle, formatStringToDoc)

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
        progDescDoc (formatStringToDoc "Scan an image for vulnerabilities")
    )

cliParser :: Parser ContainerAnalyzeOptions
cliParser =
  ContainerAnalyzeOptions
    <$> commonOpts
    <*> flagOpt
      NoUpload
      ( applyFossaStyle
          <> long "output"
          <> short 'o'
          <> helpDoc (formatStringToDoc "Output results to stdout instead of uploading to FOSSA")
      )
    <*> flagOpt JsonOutput (applyFossaStyle <> long "json" <> helpDoc (formatStringToDoc "Output project metadata as JSON to the console. This is useful for communicating with the FOSSA API."))
    <*> optional
      ( strOption
          ( applyFossaStyle
              <> long "branch"
              <> short 'b'
              <> helpDoc branchHelp
          )
      )
    <*> metadataOpts
    <*> imageTextArg
    <*> switch (applyFossaStyle <> long "experimental-scanner" <> helpDoc (formatStringToDoc "Uses experimental fossa native container scanner") <> hidden)
    <*> switch (applyFossaStyle <> long "only-system-deps" <> helpDoc (formatStringToDoc "Only analyzes system dependencies (e.g. apk, dep, rpm)"))

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
      metaMerged <- collectAPIMetadata maybeCfgFile containerMetadata
      pure $ UploadScan apiOpts metaMerged

collectFilters :: Maybe ConfigFile -> AllFilters
collectFilters maybeConfig = do
  let cfgFileFilters = maybe mempty collectConfigFileFilters maybeConfig
  if isMempty cfgFileFilters
    then mempty
    else cfgFileFilters
