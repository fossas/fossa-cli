{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Config.Container.Dump (
  ContainerDumpScanOptions (..),
  ContainerDumpScanConfig (..),
  mergeOpts,
  cliParser,
  subcommand,
) where

import App.Fossa.Config.ConfigFile (ConfigFile)
import App.Fossa.Config.Container.Common (ImageText, imageTextArg)
import App.Fossa.Config.EnvironmentVars (EnvVars)
import Control.Effect.Lift (Has, Lift, sendIO)
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
import Path (Abs, File, Path)
import Path.IO (getCurrentDir, resolveFile)

subcommand :: (ContainerDumpScanOptions -> a) -> Mod CommandFields a
subcommand f =
  command
    "dump-scan"
    ( info (f <$> cliParser) $
        progDesc "Capture syft output for debugging"
    )

data ContainerDumpScanOptions = ContainerDumpScanOptions
  { dumpScanOutputFile :: Maybe FilePath
  , dumpScanImage :: ImageText
  }

data ContainerDumpScanConfig = ContainerDumpScanConfig
  { outputFile :: Maybe (Path Abs File)
  , dumpImageLocator :: ImageText
  }
  deriving (Eq, Ord, Show)

mergeOpts ::
  Has (Lift IO) sig m =>
  Maybe ConfigFile ->
  EnvVars ->
  ContainerDumpScanOptions ->
  m ContainerDumpScanConfig
mergeOpts _ _ ContainerDumpScanOptions{..} = do
  curdir <- sendIO getCurrentDir
  maybeOut <- case dumpScanOutputFile of
    Nothing -> pure Nothing
    Just fp -> sendIO $ Just <$> resolveFile curdir fp
  pure $ ContainerDumpScanConfig maybeOut dumpScanImage

cliParser :: Parser ContainerDumpScanOptions
cliParser =
  ContainerDumpScanOptions
    <$> optional
      ( strOption
          ( short 'o'
              <> long "output-file"
              <> help "File to write the scan data (omit for stdout)"
          )
      )
    <*> imageTextArg
