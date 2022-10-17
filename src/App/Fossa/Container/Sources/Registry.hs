module App.Fossa.Container.Sources.Registry (
  analyzeFromRegistry,
  listTargetsFromRegistry,
  revisionFromRegistry,
) where

import App.Fossa.Container.Sources.DockerArchive (analyzeFromDockerArchive, listTargetsFromDockerArchive, revisionFromDockerArchive)
import Container.Docker.Credentials (useCredentialFromConfig)
import Container.Docker.SourceParser (RegistryImageSource (RegistryImageSource), defaultRegistry)
import Container.Types (ContainerScan)
import Control.Carrier.ContainerRegistryApi (runContainerRegistryApi)
import Control.Carrier.Lift (Lift)
import Control.Effect.ContainerRegistryApi (exportImage)
import Control.Effect.Debug (Debug, Has)
import Control.Effect.Diagnostics (Diagnostics, recover)
import Control.Effect.Path (withSystemTempDir)
import Control.Effect.Telemetry (Telemetry)
import Data.Maybe (fromMaybe)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Discovery.Filters (AllFilters)
import Effect.Exec (Exec)
import Effect.Logger (Logger, logInfo, pretty)
import Effect.ReadFS (ReadFS)
import Path (Abs, File, Path)

runFromRegistry ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  RegistryImageSource ->
  (Path Abs File -> m b) ->
  m b
runFromRegistry imgSrc f = do
  imgSrc' <- enrichCreds
  withSystemTempDir "fossa-container-registry-tmp" $ \dir -> do
    logInfo $ "Inferred registry source: " <> pretty imgSrc'
    tempTarFile <- runContainerRegistryApi $ exportImage imgSrc' dir
    logInfo . pretty $ "Analyzing exported docker archive: " <> toText tempTarFile
    f tempTarFile
  where
    hasCred :: RegistryImageSource -> Bool
    hasCred (RegistryImageSource _ _ (Just _) _ _ _) = True
    hasCred _ = False

    isOfficialImageSrc :: RegistryImageSource -> Bool
    isOfficialImageSrc (RegistryImageSource host _ _ repo _ _) =
      (host == defaultRegistry) && Text.isPrefixOf "library/" repo

    enrichCreds ::
      ( Has Diagnostics sig m
      , Has Exec sig m
      , Has (Lift IO) sig m
      , Has ReadFS sig m
      ) =>
      m RegistryImageSource
    enrichCreds =
      if (hasCred imgSrc || isOfficialImageSrc imgSrc)
        then pure imgSrc
        else do
          imgSrcEnriched <- recover $ useCredentialFromConfig imgSrc
          pure $ fromMaybe imgSrc imgSrcEnriched

analyzeFromRegistry ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has Telemetry sig m
  , Has Debug sig m
  , Has ReadFS sig m
  ) =>
  Bool ->
  AllFilters ->
  RegistryImageSource ->
  m ContainerScan
analyzeFromRegistry systemDepsOnly filters img =
  runFromRegistry
    img
    $ analyzeFromDockerArchive systemDepsOnly filters

listTargetsFromRegistry ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Telemetry sig m
  ) =>
  RegistryImageSource ->
  m ()
listTargetsFromRegistry img =
  runFromRegistry
    img
    listTargetsFromDockerArchive

revisionFromRegistry ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  RegistryImageSource ->
  m (Text, Text)
revisionFromRegistry img = runFromRegistry img revisionFromDockerArchive
