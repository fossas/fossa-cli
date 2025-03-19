{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Container.Sources.DockerEngine (
  analyzeFromDockerEngine,
  listTargetsFromDockerEngine,
  revisionFromDockerEngine,
) where

import App.Fossa.Config.Analyze (WithoutDefaultFilters)
import App.Fossa.Container.Sources.DockerArchive (analyzeFromDockerArchive, listTargetsFromDockerArchive, revisionFromDockerArchive)
import Container.Types (ContainerScan)
import Control.Carrier.DockerEngineApi (runDockerEngineApi)
import Control.Carrier.Lift (Lift)
import Control.Effect.Debug (Debug, Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.DockerEngineApi (getDockerImage)
import Control.Effect.Path (withSystemTempDir)
import Control.Effect.Telemetry (Telemetry)
import Data.Flag (Flag)
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Discovery.Filters (AllFilters)
import Effect.Exec (Exec)
import Effect.Logger (Logger, logInfo, pretty)
import Path (Abs, File, Path, mkRelFile, (</>))

runFromDockerEngine ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  Text ->
  Text ->
  (Path Abs File -> m b) ->
  m b
runFromDockerEngine engineHost imgTag f = do
  withSystemTempDir "fossa-docker-engine-tmp" $ \dir -> do
    let tempTarFile = dir </> $(mkRelFile "image.tar")

    logInfo . pretty $
      "Exporting docker image to temp file: "
        <> toString tempTarFile
        <> "! This may take a while!"
    runDockerEngineApi engineHost $ getDockerImage imgTag tempTarFile

    logInfo . pretty $ "Analyzing exported docker archive: " <> toText tempTarFile
    f tempTarFile

analyzeFromDockerEngine ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m -- May not exec dynamic strategies. TODO: Remove this and convert the BerkeleyDB driver to FFI.
  , Has Logger sig m
  , Has Telemetry sig m
  , Has Debug sig m
  ) =>
  Bool ->
  AllFilters ->
  Flag WithoutDefaultFilters ->
  Text ->
  Text ->
  m ContainerScan
analyzeFromDockerEngine systemDepsOnly filters withoutDefaultFilters engineHost imgTag =
  runFromDockerEngine
    engineHost
    imgTag
    $ analyzeFromDockerArchive systemDepsOnly filters withoutDefaultFilters

listTargetsFromDockerEngine ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m -- May not exec dynamic strategies. TODO: Remove this and convert the BerkeleyDB driver to FFI.
  , Has Logger sig m
  , Has Telemetry sig m
  ) =>
  Text ->
  Text ->
  m ()
listTargetsFromDockerEngine engineHost imgTag =
  runFromDockerEngine
    engineHost
    imgTag
    listTargetsFromDockerArchive

revisionFromDockerEngine ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  Text ->
  Text ->
  m (Text, Text)
revisionFromDockerEngine engineHost imgTag =
  runFromDockerEngine
    engineHost
    imgTag
    revisionFromDockerArchive
