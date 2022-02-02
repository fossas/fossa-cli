{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.VPS.AOSP (
  AOSPNoticeConfig (..),
  AOSPNoticeOpts (..),
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
import App.Fossa.VPS.Types (
  NinjaFilePaths (NinjaFilePaths),
  NinjaScanID (NinjaScanID),
 )
import App.Types (
  BaseDir,
  OverrideProject (OverrideProject),
  ProjectMetadata,
  ProjectRevision,
 )
import App.Util (validateFile)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  fatalOnIOException,
 )
import Control.Effect.Lift (Lift, sendIO)
import Data.String.Conversion (ToString (toString))
import Data.Text (Text)
import Data.Text qualified as Text
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
  progDesc,
  strOption,
 )
import Path (Abs, File, Path)

data AOSPNoticeConfig = AOSPNoticeConfig
  { aospApiOpts :: ApiOpts
  , aospBaseDir :: BaseDir
  , aospMetadata :: ProjectMetadata
  , aospRevision :: ProjectRevision
  , aospSeverity :: Severity
  , ninjaFileList :: NinjaFilePaths
  , ninjaScanId :: NinjaScanID
  }
  deriving (Eq, Ord, Show)

data AOSPNoticeOpts = AOSPNoticeOpts
  { aospCommons :: CommonOpts
  , aospCliBaseDir :: FilePath
  , aospNinjaScanID :: Text
  , aospNinjaFileList :: Text
  , aospNinjaScanMeta :: ProjectMetadata
  }
  deriving (Eq, Ord, Show)

mergeOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  Maybe ConfigFile ->
  EnvVars ->
  AOSPNoticeOpts ->
  m AOSPNoticeConfig
mergeOpts cfgfile envvars AOSPNoticeOpts{..} = do
  let metadata = collectProjectMetadata cfgfile aospNinjaScanMeta
      severity = if (optDebug aospCommons) then SevDebug else SevInfo
      scanId = NinjaScanID aospNinjaScanID
      apiopts = collectApiOpts cfgfile envvars aospCommons
      basedir = collectBaseDir aospCliBaseDir
      filepaths = NinjaFilePaths <$> parseCommaSeparatedFileArg aospNinjaFileList
      revision =
        collectRevisionData' basedir cfgfile WriteOnly $
          OverrideProject
            (optProjectName aospCommons)
            (optProjectRevision aospCommons)
            Nothing
  AOSPNoticeConfig
    <$> apiopts
    <*> basedir
    <*> pure metadata
    <*> revision
    <*> pure severity
    <*> filepaths
    <*> pure scanId

parseCommaSeparatedFileArg :: (Has Diagnostics sig m, Has (Lift IO) sig m) => Text -> m [Path Abs File]
parseCommaSeparatedFileArg arg =
  fatalOnIOException "Parsing comma-separated file paths" $
    traverse (sendIO . validateFile . toString) $ Text.splitOn "," arg

subcommand :: (AOSPNoticeOpts -> a) -> Mod CommandFields a
subcommand f =
  command
    "aosp-notice-file"
    ( info
        (f <$> cliParser)
        (progDesc "Upload information required to generate NOTICE files for this build to FOSSA")
    )

cliParser :: Parser AOSPNoticeOpts
cliParser =
  AOSPNoticeOpts
    <$> commonOpts
    <*> baseDirArg
    <*> strOption (long "scan-id" <> help "ID of the scan to which notice content should be added. Reported by `analyze` upon completion.")
    <*> strOption (long "ninja-files" <> help "A comma-separated list of ninja files to parse for build graph information.")
    <*> metadataOpts
