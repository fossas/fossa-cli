{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Container.Sources.Registry (
  analyzeFromRegistry,
  listTargetsFromRegistry,
  revisionFromRegistry,
  runWithCirceReexport,
) where

import App.Fossa.Config.Analyze (WithoutDefaultFilters)
import App.Fossa.Container.Sources.Circe (circeReexportCommand)
import App.Fossa.Container.Sources.DockerArchive (analyzeFromDockerArchive, listTargetsFromDockerArchive, revisionFromDockerArchive)
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
    tempTarFile <- runWithCirceReexport imgSrc' dir
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
-- Falls back to the standard registry @exportImage@ if circe fails.
runWithCirceReexport ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  RegistryImageSource ->
  Path Abs Dir ->
  m (Path Abs File)
runWithCirceReexport imgSrc tempDir = do
  let tarballPath = tempDir </> $(mkRelFile "image.tar")

  circeResult <- context "Using circe reexport" $
    warnThenRecover @Text "Failed to use circe reexport, falling back to direct registry API" $ do
      withCirceBinary $ \paths -> do
        logInfo "Exporting normalized container image"
        _ <- execThrow' $ circeReexportCommand paths imgSrc (toText tarballPath)
        logDebug $ "Circe reexport completed successfully to: " <> pretty (toText tarballPath)
        pure tarballPath

  case circeResult of
    Just path -> pure path
    Nothing -> do
      logInfo "Falling back to registry API for plain container image"
      runContainerRegistryApi $ exportImage imgSrc tempDir

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
    $ analyzeFromDockerArchive systemDepsOnly filters withoutDefaultFilters

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
