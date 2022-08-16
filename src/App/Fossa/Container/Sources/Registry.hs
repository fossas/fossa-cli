module App.Fossa.Container.Sources.Registry (analyzeFromRegistry) where

import App.Fossa.Container.Sources.DockerTarball (analyzeExportedTarball)
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
import Data.String.Conversion (toString)
import Data.Text qualified as Text
import Effect.Exec (Exec)
import Effect.Logger (Logger, logInfo, pretty)
import Effect.ReadFS (ReadFS)

analyzeFromRegistry ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has Telemetry sig m
  , Has ReadFS sig m
  , Has Debug sig m
  ) =>
  RegistryImageSource ->
  m ContainerScan
analyzeFromRegistry imgSrc = do
  imgSrc' <- enrichCreds
  withSystemTempDir "fossa-container-registry-tmp" $ \dir -> do
    logInfo $ "Inferred registry source: " <> pretty imgSrc'
    tempTarFile <- runContainerRegistryApi $ exportImage imgSrc' dir
    logInfo . pretty $ "Analyzing exported container tarball: " <> toString tempTarFile
    analyzeExportedTarball tempTarFile
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
