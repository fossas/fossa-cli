{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Container (
  containerSubCommand,
) where

import App.Fossa.Config.Container (
  ContainerAnalyzeConfig (usesExperimentalScanner),
  ContainerCommand,
  ContainerDumpScanConfig (ContainerDumpScanConfig),
  ContainerParseFileConfig (ContainerParseFileConfig),
  ContainerScanConfig (..),
 )
import App.Fossa.Config.Container qualified as Config
import App.Fossa.Container.Analyze qualified as Analyze
import App.Fossa.Container.AnalyzeNative qualified as AnalyzeNative
import App.Fossa.Container.Scan (SyftResponse, syftCommand, toContainerScan)
import App.Fossa.Container.Test qualified as Test
import App.Fossa.EmbeddedBinary (withSyftBinary)
import App.Fossa.Subcommand (SubCommand)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  fromEitherShow,
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Telemetry (Telemetry)
import Data.Aeson (FromJSON (parseJSON), Value, encode)
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy qualified as BL
import Data.String.Conversion (decodeUtf8, toString)
import Effect.Exec (Exec, execThrow')
import Effect.Logger (
  Logger,
  logDebug,
  logInfo,
  logStdout,
 )
import Effect.ReadFS (ReadFS, readContentsJson)

containerSubCommand :: SubCommand ContainerCommand ContainerScanConfig
containerSubCommand = Config.mkSubCommand dispatch

dispatch ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Telemetry sig m
  ) =>
  ContainerScanConfig ->
  m ()
dispatch = \case
  AnalyzeCfg cfg ->
    if usesExperimentalScanner cfg
      then AnalyzeNative.analyzeExperimental cfg
      else Analyze.analyze cfg
  TestCfg cfg -> Test.test cfg
  DumpCfg cfg -> dumpSyftScan cfg
  ParseCfg cfg -> parseSyftOutput cfg

parseSyftOutput ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  ContainerParseFileConfig ->
  m ()
parseSyftOutput (ContainerParseFileConfig path) = do
  logDebug "Reading JSON file"
  rawvalue <- readContentsJson @Value path
  logDebug "Parsing JSON contents"
  response <- fromEitherShow $ parseEither (parseJSON @SyftResponse) rawvalue
  logDebug "Converting to FOSSA payload"
  payload <- toContainerScan response
  logInfo "Payload is valid!"

  logStdout . decodeUtf8 $ encode payload

dumpSyftScan ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  ContainerDumpScanConfig ->
  m ()
dumpSyftScan ContainerDumpScanConfig{..} = withSyftBinary $ \syft -> do
  syftOutput <- execThrow' $ syftCommand syft dumpImageLocator
  let writer :: BL.ByteString -> IO ()
      writer = maybe BL.putStr (BL.writeFile . toString) outputFile
  sendIO $ writer syftOutput
