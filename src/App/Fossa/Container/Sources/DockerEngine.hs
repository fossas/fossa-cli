{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Container.Sources.DockerEngine (analyzeFromDockerEngine) where

import App.Fossa.Container.Sources.DockerArchive (analyzeFromDockerArchive)
import Container.Types (ContainerScan)
import Control.Carrier.DockerEngineApi (runDockerEngineApi)
import Control.Carrier.Lift (Lift)
import Control.Effect.Debug (Debug, Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.DockerEngineApi (getDockerImage)
import Control.Effect.Path (withSystemTempDir)
import Control.Effect.Telemetry (Telemetry)
import Data.String.Conversion (toString)
import Data.Text (Text)
import Effect.Exec (Exec)
import Effect.Logger (Logger, logInfo, pretty)
import Path (mkRelFile, (</>))

analyzeFromDockerEngine ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has Telemetry sig m
  , Has Debug sig m
  ) =>
  Text ->
  Text ->
  m ContainerScan
analyzeFromDockerEngine imgTag dockerHost = do
  withSystemTempDir "fossa-docker-engine-tmp" $ \dir -> do
    let tempTarFile = dir </> $(mkRelFile "image.tar")

    logInfo . pretty $
      "Exporting docker image to temp file: "
        <> toString tempTarFile
        <> "! This may take a while!"
    runDockerEngineApi dockerHost $ getDockerImage imgTag tempTarFile

    logInfo . pretty $ "Analyzing exported docker tarball: " <> toString tempTarFile
    analyzeFromDockerArchive tempTarFile
