{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Container.Analyze (
  NoUpload (..),
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
  collectRevisionOverride,
  commonOpts,
  metadataOpts,
 )
import App.Fossa.Config.ConfigFile (ConfigFile)
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
import Data.Text (Text)
import Effect.Logger (Severity (SevDebug, SevInfo))
import GHC.Generics (Generic)
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  command,
  help,
  info,
  long,
  optional,
  progDesc,
  short,
  strOption,
  switch,
 )

data NoUpload = NoUpload

data ContainerAnalyzeConfig = ContainerAnalyzeConfig
  { scanDestination :: ScanDestination
  , revisionOverride :: OverrideProject
  , imageLocator :: ImageText
  , -- \* For Experimental Scanner
    usesExperimentalScanner :: Bool
  , dockerHost :: Text
  , arch :: Text
  , severity :: Severity
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ContainerAnalyzeConfig where
  toEncoding = genericToEncoding defaultOptions

data ContainerAnalyzeOptions = ContainerAnalyzeOptions
  { analyzeCommons :: CommonOpts
  , containerNoUpload :: Flag NoUpload
  , containerBranch :: Maybe Text
  , containerMetadata :: ProjectMetadata
  , containerAnalyzeImage :: ImageText
  , containerExperimentalScanner :: Bool
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
    <*> optional
      ( strOption
          ( long "branch"
              <> short 'b'
              <> help "this repository's current branch (default: current VCS branch)"
          )
      )
    <*> metadataOpts
    <*> imageTextArg
    <*> switch (long "experimental-scanner" <> help "Uses experimental fossa native container scanner")

mergeOpts ::
  Has Diagnostics sig m =>
  Maybe ConfigFile ->
  EnvVars ->
  ContainerAnalyzeOptions ->
  m ContainerAnalyzeConfig
mergeOpts cfgfile envvars cliOpts@ContainerAnalyzeOptions{..} = do
  let scanDest = collectScanDestination cfgfile envvars cliOpts
      severity = getSeverity cliOpts
      imageLoc = containerAnalyzeImage
      arch = collectArch
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
    <*> pure containerExperimentalScanner
    <*> collectDockerHost envvars
    <*> pure arch
    <*> pure severity

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
      apiOpts <- collectApiOpts maybeCfgFile envvars analyzeCommons
      let metaMerged = collectAPIMetadata maybeCfgFile containerMetadata
      pure $ UploadScan apiOpts metaMerged
