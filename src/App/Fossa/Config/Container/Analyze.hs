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
  CommonOpts (optProjectName, optProjectRevision),
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
  imageTextArg,
 )
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Types (
  OverrideProject (OverrideProject),
  ProjectMetadata,
 )
import Control.Effect.Diagnostics (Diagnostics, Has)
import Data.Flag (Flag, flagOpt, fromFlag)
import Data.Text (Text)
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
 )

data NoUpload = NoUpload

data ContainerAnalyzeConfig = ContainerAnalyzeConfig
  { scanDestination :: ScanDestination
  , revisionOverride :: OverrideProject
  , imageLocator :: ImageText
  }
  deriving (Eq, Ord, Show)

data ContainerAnalyzeOptions = ContainerAnalyzeOptions
  { analyzeCommons :: CommonOpts
  , containerNoUpload :: Flag NoUpload
  , containerBranch :: Maybe Text
  , containerMetadata :: ProjectMetadata
  , containerAnalyzeImage :: ImageText
  }

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

mergeOpts ::
  Has Diagnostics sig m =>
  Maybe ConfigFile ->
  EnvVars ->
  ContainerAnalyzeOptions ->
  m ContainerAnalyzeConfig
mergeOpts cfgfile envvars cliOpts@ContainerAnalyzeOptions{..} = do
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
      apiOpts <- collectApiOpts maybeCfgFile envvars analyzeCommons
      let metaMerged = collectAPIMetadata maybeCfgFile containerMetadata
      pure $ UploadScan apiOpts metaMerged
