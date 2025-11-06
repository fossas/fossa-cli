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

import App.Docs (fossaContainerAnalyzeDefaultFilterDocUrl)
import App.Fossa.Config.Analyze (WithoutDefaultFilters, branchHelp, withoutDefaultFilterParser)
import App.Fossa.Config.Common (
  CommonOpts (CommonOpts, optDebug, optProjectName, optProjectRevision),
  DestinationMeta (..),
  OutputStyle (..),
  ScanDestination (..),
  collectAPIMetadata,
  collectApiOpts,
  collectConfigFileFilters,
  collectRevisionOverride,
  commonOpts,
  metadataOpts,
  outputStyleArgs,
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
import Data.Flag (Flag, flagOpt)
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
import Style (applyFossaStyle, formatStringToDoc, stringToHelpDoc)

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
  , onlySystemDeps :: Bool
  , filterSet :: AllFilters
  , withoutDefaultFilters :: Flag WithoutDefaultFilters
  , debugDir :: Maybe FilePath
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ContainerAnalyzeConfig where
  toEncoding = genericToEncoding defaultOptions

data ContainerAnalyzeOptions = ContainerAnalyzeOptions
  { analyzeCommons :: CommonOpts
  , containerOutputStyle :: OutputStyle
  , containerJsonOutput :: Flag JsonOutput
  , containerBranch :: Maybe Text
  , containerMetadata :: ProjectMetadata
  , containerAnalyzeImage :: ImageText
  , containerExperimentalScanner :: Bool
  , containerExperimentalOnlySysDependencies :: Bool
  , containerWithoutDefaultFilters :: Flag WithoutDefaultFilters
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
    <*> outputStyleArgs
    <*> flagOpt JsonOutput (applyFossaStyle <> long "json" <> stringToHelpDoc "Output project metadata as JSON to the console. This is useful for communicating with the FOSSA API.")
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
    <*> switch (applyFossaStyle <> long "experimental-scanner" <> stringToHelpDoc "Uses experimental fossa native container scanner" <> hidden)
    <*> switch (applyFossaStyle <> long "only-system-deps" <> stringToHelpDoc "Only analyzes system dependencies (e.g. apk, dep, rpm)")
    <*> withoutDefaultFilterParser fossaContainerAnalyzeDefaultFilterDocUrl

mergeOpts ::
  (Has Diagnostics sig m) =>
  Maybe FilePath ->
  Maybe ConfigFile ->
  EnvVars ->
  ContainerAnalyzeOptions ->
  m ContainerAnalyzeConfig
mergeOpts maybeDebugDir cfgfile envvars cliOpts@ContainerAnalyzeOptions{..} = do
  let scanDest = collectScanDestination cfgfile envvars cliOpts
      imageLoc = containerAnalyzeImage
      jsonOutput = containerJsonOutput
      arch = collectArch
      onlySystemDeps = containerExperimentalOnlySysDependencies
      scanFilters = collectFilters cfgfile
      withoutDefaultFilters = containerWithoutDefaultFilters

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
    <*> pure onlySystemDeps
    <*> pure scanFilters
    <*> pure withoutDefaultFilters
    <*> pure maybeDebugDir

collectScanDestination ::
  (Has Diagnostics sig m) =>
  Maybe ConfigFile ->
  EnvVars ->
  ContainerAnalyzeOptions ->
  m ScanDestination
collectScanDestination maybeCfgFile envvars ContainerAnalyzeOptions{..} =
  case containerOutputStyle of
    Output -> pure OutputStdout
    TeeOutput -> getScanUploadDest OutputAndUpload
    Default -> getScanUploadDest UploadScan
  where
    getScanUploadDest constructor = do
      apiOpts <- collectApiOpts maybeCfgFile envvars analyzeCommons
      metaMerged <- collectAPIMetadata maybeCfgFile containerMetadata
      pure $ constructor (DestinationMeta (apiOpts, metaMerged))

collectFilters :: Maybe ConfigFile -> AllFilters
collectFilters maybeConfig = do
  let cfgFileFilters = maybe mempty collectConfigFileFilters maybeConfig
  if isMempty cfgFileFilters
    then mempty
    else cfgFileFilters
