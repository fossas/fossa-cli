{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.SBOM.Analyze (
  NoUpload (..),
  JsonOutput (..),
  SBOMAnalyzeConfig (..),
  SBOMAnalyzeOptions (..),
  SBOMScanDestination (..),
  cliParser,
  mergeOpts,
  subcommand,
) where

import App.Fossa.Config.Common (
  CacheAction (..),
  CommonOpts (..),
  collectApiOpts,
  collectRevisionOverride,
  commonOpts,
 )
import App.Fossa.Config.ConfigFile
import App.Fossa.Config.EnvironmentVars (EnvVars)
import App.Fossa.Config.SBOM.Common (SBOMFile, getProjectRevision, sbomFileArg)
import App.Fossa.Subcommand (GetSeverity, getSeverity)
import App.Types (
  BaseDir (BaseDir),
  DependencyRebuild (..),
  OverrideProject (OverrideProject),
  ProjectRevision,
 )
import Control.Applicative (optional)
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Effect.Lift (Lift)
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding)
import Data.Aeson.Types (ToJSON (toEncoding))
import Data.Flag (Flag, flagOpt, fromFlag)
import Data.Text (Text)
import Effect.Logger (Severity (SevDebug, SevInfo))
import Effect.ReadFS (ReadFS, getCurrentDir)
import Fossa.API.Types (ApiOpts)
import GHC.Generics (Generic)
import Options.Applicative (CommandFields, Mod, Parser, command, info, long, short)
import Options.Applicative.Builder (progDescDoc, strOption)
import Style (applyFossaStyle, formatStringToDoc, stringToHelpDoc)

data NoUpload = NoUpload
data JsonOutput = JsonOutput deriving (Generic)
data SBOMScanDestination
  = SBOMUploadScan ApiOpts
  | SBOMOutputStdout
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SBOMScanDestination where
  toEncoding = genericToEncoding defaultOptions

data SBOMAnalyzeConfig = SBOMAnalyzeConfig
  { sbomBaseDir :: BaseDir
  , sbomApiOpts :: ApiOpts
  , sbomPath :: SBOMFile
  , sbomRebuild :: DependencyRebuild
  , sbomTeam :: Maybe Text
  , sbomRevision :: ProjectRevision
  , debugDir :: Maybe FilePath
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SBOMAnalyzeConfig where
  toEncoding = genericToEncoding defaultOptions

data SBOMAnalyzeOptions = SBOMAnalyzeOptions
  { analyzeCommons :: App.Fossa.Config.Common.CommonOpts
  , team :: Maybe Text
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
        progDescDoc (formatStringToDoc "Upload and analyze an SBOM file in FOSSA")
    )

data ForceRescan = ForceRescan deriving (Generic)

cliParser :: Parser SBOMAnalyzeOptions
cliParser =
  SBOMAnalyzeOptions
    <$> App.Fossa.Config.Common.commonOpts
    <*> optional (strOption (applyFossaStyle <> long "team" <> short 'T' <> stringToHelpDoc "This SBOM's team inside your organization"))
    <*> flagOpt ForceRescan (applyFossaStyle <> long "force-rescan" <> stringToHelpDoc "Force sbom file to be rescanned even if the revision has been previously analyzed by FOSSA.")
    <*> sbomFileArg

mergeOpts ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  ) =>
  Maybe FilePath ->
  Maybe ConfigFile ->
  EnvVars ->
  SBOMAnalyzeOptions ->
  m SBOMAnalyzeConfig
mergeOpts maybeDebugDir cfgfile envvars SBOMAnalyzeOptions{..} = do
  baseDir <- getCurrentDir
  let fileLoc = sbomFile

      revOverride =
        collectRevisionOverride cfgfile $
          OverrideProject
            (optProjectName analyzeCommons)
            (optProjectRevision analyzeCommons)
            (Nothing)

      forceRescans = if fromFlag ForceRescan forceRescan then DependencyRebuildInvalidateCache else DependencyRebuildReuseCache
  apiOpts <- App.Fossa.Config.Common.collectApiOpts cfgfile envvars analyzeCommons
  revision <- getProjectRevision fileLoc revOverride WriteOnly
  pure $
    SBOMAnalyzeConfig
      { sbomBaseDir = (BaseDir baseDir)
      , sbomApiOpts = apiOpts
      , sbomPath = fileLoc
      , sbomRebuild = forceRescans
      , sbomTeam = team
      , sbomRevision = revision
      , debugDir = maybeDebugDir
      }
