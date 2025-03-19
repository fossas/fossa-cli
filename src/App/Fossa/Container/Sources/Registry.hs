{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Container.Sources.Registry (
  analyzeFromRegistry,
  listTargetsFromRegistry,
  revisionFromRegistry,
  runWithCirceReexport,
) where

import App.Fossa.Config.Analyze (WithoutDefaultFilters)
import App.Fossa.Config.Container.Common (ImageText, unImageText)
import App.Fossa.Container.Sources.Circe (circeReexportCommand)
import App.Fossa.Container.Sources.DockerArchive (analyzeFromNormalizedDockerArchive, listTargetsFromDockerArchive, revisionFromDockerArchive)
import App.Fossa.EmbeddedBinary (withCirceBinary)
import Container.Docker.Credentials (useCredentialFromConfig)
import Container.Docker.SourceParser (RegistryImageSource (RegistryImageSource), defaultRegistry)
import Container.Types (ContainerScan)
import Control.Carrier.ContainerRegistryApi (runContainerRegistryApi)
import Control.Carrier.Lift (Lift)
import Control.Effect.ContainerRegistryApi (exportImage)
import Control.Effect.Debug (Debug, Has)
import Control.Effect.Diagnostics (Diagnostics, context, recover, warnThenRecover)
import Control.Effect.Path (withSystemTempDir)
import Control.Effect.Telemetry (Telemetry)
import Data.Flag (Flag)
import Data.Maybe (fromMaybe)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Discovery.Filters (AllFilters)
import Effect.Exec (Exec, execThrow')
import Effect.Logger (Logger, logDebug, logInfo, pretty)
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, File, Path, mkRelFile, (</>))

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

-- | Attempts to use circe reexport for container image export.
-- Returns the path on disk to the normalized tarball exported by Circe.
-- If Circe could not fetch the image, warns and returns @Nothing@.
runWithCirceReexport ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  ImageText ->
  Path Abs Dir ->
  m (Maybe (Path Abs File))
runWithCirceReexport img dir = do
  let tarballPath = dir </> $(mkRelFile "image.tar")
  context "Using circe reexport" $
    warnThenRecover @Text "Failed to use circe reexport, falling back to direct registry API" $ do
      withCirceBinary $ \paths -> do
        logInfo . pretty $ "Exporting normalized container image for: " <> unImageText img
        _ <- execThrow' $ circeReexportCommand paths img (toText tarballPath)
        logDebug $ "Circe reexport completed successfully to: " <> pretty (toText tarballPath)
        pure tarballPath

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
  Flag WithoutDefaultFilters ->
  RegistryImageSource ->
  m ContainerScan
analyzeFromRegistry systemDepsOnly filters withoutDefaultFilters img =
  runFromRegistry
    img
    $ analyzeFromNormalizedDockerArchive systemDepsOnly filters withoutDefaultFilters

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
