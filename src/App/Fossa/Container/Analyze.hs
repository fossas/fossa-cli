{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Container.Analyze (
  analyze,
) where

import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Container.Scan (extractRevision, runSyft, toContainerScan)
import App.Fossa.FossaAPIV1 (UploadResponse (uploadError, uploadLocator), uploadContainerScan)
import App.NewFossa.Config.Common (
  ScanDestination (OutputStdout, UploadScan),
 )
import App.NewFossa.Config.Container (
  ContainerAnalyzeConfig (
    ContainerAnalyzeConfig,
    imageLocator,
    revisionOverride,
    scanDestination
  ),
 )
import App.Types (ProjectRevision (..))
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Data.Aeson (encode)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.String.Conversion (decodeUtf8)
import Effect.Logger (
  Has,
  Logger,
  Pretty (pretty),
  logDebug,
  logError,
  logInfo,
  logStdout,
  viaShow,
 )
import Srclib.Types (parseLocator)

analyze ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  ContainerAnalyzeConfig ->
  m ()
analyze ContainerAnalyzeConfig{..} = do
  logDebug "Running embedded syft binary"
  containerScan <- runSyft imageLocator >>= toContainerScan
  case scanDestination of
    OutputStdout -> logStdout . decodeUtf8 $ encode containerScan
    UploadScan apiOpts projectMeta -> do
      let revision = extractRevision revisionOverride containerScan
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using project revision: `" <> pretty (projectRevision revision) <> "`")
      let branchText = fromMaybe "No branch (detached HEAD)" $ projectBranch revision
      logInfo ("Using branch: `" <> pretty branchText <> "`")

      resp <- uploadContainerScan apiOpts revision projectMeta containerScan

      buildUrl <- getFossaBuildUrl revision apiOpts . parseLocator $ uploadLocator resp
      logInfo "View FOSSA Report:"
      logInfo ("  " <> pretty buildUrl)
      -- Report non-critical errors
      traverse_ (\err -> logError $ "FOSSA error: " <> viaShow err) (uploadError resp)
