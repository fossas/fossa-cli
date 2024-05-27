{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.SBOM.Analyze (
  NoUpload (..),
  JsonOutput (..),
  SBOMAnalyzeConfig (..),
  SBOMAnalyzeOptions (..),
  cliParser,
  mergeOpts,
  subcommand,
) where

import App.Fossa.Config.Common (
  CommonOpts (..),
  ScanDestination (..),
  collectAPIMetadata,
  collectApiOpts,
  collectRevisionOverride,
  commonOpts,
  metadataOpts,
 )
import App.Fossa.Config.ConfigFile
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Config.SBOM.Common (SBOMFile, sbomFileArg)
import App.Fossa.Subcommand (GetSeverity, getSeverity)
import App.Types (
  BaseDir (BaseDir),
  DependencyRebuild (..),
  OverrideProject (OverrideProject),
  ProjectMetadata,
 )
import Control.Effect.Diagnostics (Diagnostics, Has)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding)
import Data.Aeson.Types (ToJSON (toEncoding))
import Data.Flag (Flag, flagOpt, fromFlag)
import Effect.Logger (Severity (SevDebug, SevInfo))
import Effect.ReadFS (ReadFS, getCurrentDir)
import GHC.Generics (Generic)
import Options.Applicative (CommandFields, Mod, Parser, command, info, long)
import Options.Applicative.Builder (progDescDoc)
import Style (applyFossaStyle, formatStringToDoc, stringToHelpDoc)

data NoUpload = NoUpload
data JsonOutput = JsonOutput deriving (Generic)

data SBOMAnalyzeConfig = SBOMAnalyzeConfig
  { sbomBaseDir :: BaseDir
  , sbomScanDestination :: App.Fossa.Config.Common.ScanDestination
  , revisionOverride :: OverrideProject
  , sbomPath :: SBOMFile
  , severity :: Severity
  , sbomRebuild :: DependencyRebuild
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SBOMAnalyzeConfig where
  toEncoding = genericToEncoding defaultOptions

data SBOMAnalyzeOptions = SBOMAnalyzeOptions
  { analyzeCommons :: App.Fossa.Config.Common.CommonOpts
  , containerMetadata :: ProjectMetadata
  , forceRescan :: Flag ForceRescan
  , sbomFile :: SBOMFile
  }

instance GetSeverity SBOMAnalyzeOptions where
  getSeverity SBOMAnalyzeOptions{analyzeCommons = App.Fossa.Config.Common.CommonOpts{optDebug}} = if optDebug then SevDebug else SevInfo

subcommand :: (SBOMAnalyzeOptions -> a) -> Mod CommandFields a
subcommand f =
  command
    "analyze"
    ( info (f <$> cliParser) $
        progDescDoc (formatStringToDoc "Scan an SBOM file")
    )

data ForceRescan = ForceRescan deriving (Generic)

cliParser :: Parser SBOMAnalyzeOptions
cliParser =
  SBOMAnalyzeOptions
    <$> App.Fossa.Config.Common.commonOpts
    <*> App.Fossa.Config.Common.metadataOpts
    <*> flagOpt ForceRescan (applyFossaStyle <> long "force-rescan" <> stringToHelpDoc "Force sbom file to be rescanned even if the revision has been previously analyzed by FOSSA.")
    <*> sbomFileArg

mergeOpts ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  SBOMAnalyzeOptions ->
  m SBOMAnalyzeConfig
mergeOpts cfgfile envvars cliOpts@SBOMAnalyzeOptions{..} = do
  baseDir <- getCurrentDir
  let scanDest = collectScanDestination cfgfile envvars cliOpts
      severity = getSeverity cliOpts
      fileLoc = sbomFile

      revOverride =
        collectRevisionOverride cfgfile $
          OverrideProject
            (optProjectName analyzeCommons)
            (optProjectRevision analyzeCommons)
            (Nothing)

      forceRescans = if fromFlag ForceRescan forceRescan then DependencyRebuildInvalidateCache else DependencyRebuildReuseCache
  (SBOMAnalyzeConfig (BaseDir baseDir) <$> scanDest)
    <*> pure revOverride
    <*> pure fileLoc
    <*> pure severity
    <*> pure forceRescans

collectScanDestination ::
  (Has Diagnostics sig m) =>
  Maybe ConfigFile ->
  EnvVars ->
  SBOMAnalyzeOptions ->
  m App.Fossa.Config.Common.ScanDestination
collectScanDestination maybeCfgFile envvars SBOMAnalyzeOptions{..} = do
  apiOpts <- App.Fossa.Config.Common.collectApiOpts maybeCfgFile envvars analyzeCommons
  metaMerged <- App.Fossa.Config.Common.collectAPIMetadata maybeCfgFile containerMetadata
  pure $ App.Fossa.Config.Common.UploadScan apiOpts metaMerged
