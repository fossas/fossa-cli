module App.Fossa.Container.AnalyzeNative (
  ContainerImageSource (..),
  analyzeExperimental,

  -- * for testing
  uploadScan,
) where

import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Analyze.Debug (collectDebugBundle)
import App.Fossa.Analyze.Upload (emitBuildWarnings)
import App.Fossa.Config.Common (
  DestinationMeta (..),
  ScanDestination (..),
  applyReleaseGroupDeprecationWarning,
 )
import App.Fossa.Config.Container.Analyze (
  ContainerAnalyzeConfig (..),
  JsonOutput (JsonOutput),
 )
import App.Fossa.Container.Scan (extractRevision, scanImage)
import App.Fossa.PreflightChecks (PreflightCommandChecks (AnalyzeChecks), preflightChecks)
import App.Types (
  ProjectMetadata,
  ProjectRevision (..),
 )
import Container.Docker.SourceParser (RegistryImageSource)
import Container.Errors (EndpointDoesNotSupportNativeContainerScan (EndpointDoesNotSupportNativeContainerScan))
import Container.Types (ContainerScan (..))
import Control.Carrier.Debug (Debug, ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Effect.Diagnostics (Diagnostics, context, fatal, fromMaybeText)
import Control.Effect.FossaApiClient (FossaApiClient, uploadNativeContainerScan)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Telemetry (Telemetry)
import Control.Monad (void, when)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Error (getSourceLocation)
import Data.Flag (Flag, fromFlag)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.String.Conversion (
  ConvertUtf8 (decodeUtf8),
  decodeUtf8,
 )
import Data.Text (Text)
import Effect.Exec (Exec)
import Effect.Logger (
  Has,
  Logger,
  Pretty (pretty),
  logError,
  logInfo,
  logStdout,
  viaShow,
 )
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (Organization (orgSupportsNativeContainerScan), UploadResponse (uploadError), uploadLocator)
import Path (Abs, File, Path)
import Srclib.Types (Locator (locatorRevision), locatorProject, renderLocator)
import System.FilePath ((</>))

data ContainerImageSource
  = DockerArchive (Path Abs File)
  | DockerEngine Text
  | Podman Text
  | Registry RegistryImageSource
  deriving (Show, Eq)

debugBundlePath :: FilePath
debugBundlePath = "fossa.debug.json"

analyzeExperimental ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Telemetry sig m
  ) =>
  ContainerAnalyzeConfig ->
  m ContainerScan
analyzeExperimental cfg = do
  let maybeDebugDir = debugDir cfg

  case maybeDebugDir of
    Just debugDir -> do
      (bundle, res) <- collectDebugBundle cfg $ Diag.errorBoundaryIO $ analyze cfg

      sendIO $ do
        let debugJsonPath = debugDir </> debugBundlePath
        BL.writeFile debugJsonPath $ Aeson.encode bundle

      Diag.rethrow res
    Nothing -> ignoreDebug $ analyze cfg

analyze ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Telemetry sig m
  , Has Debug sig m
  ) =>
  ContainerAnalyzeConfig ->
  m ContainerScan
analyze cfg = do
  scannedImage <- scanImage (filterSet cfg) (withoutDefaultFilters cfg) (onlySystemDeps cfg) (imageLocator cfg) (dockerHost cfg) (arch cfg)
  let revision = extractRevision (revisionOverride cfg) scannedImage
      logProjectInfo :: Has Logger sig m => m ()
      logProjectInfo = do
        logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
        logInfo ("Using project revision: `" <> pretty (projectRevision revision) <> "`")

        let branchText = fromMaybe "No branch (detached HEAD)" $ projectBranch revision
        logInfo ("Using branch: `" <> pretty branchText <> "`")

  let doUpload apiOpts projectMetadata = runFossaApiClient apiOpts $ do
        orgInfo <- preflightChecks $ AnalyzeChecks revision projectMetadata
        logProjectInfo
        void $ uploadScan orgInfo revision projectMetadata (jsonOutput cfg) scannedImage

  let scannedImageToStdout = do
        logProjectInfo
        logStdout . decodeUtf8 $ Aeson.encode scannedImage

  case scanDestination cfg of
    OutputStdout -> scannedImageToStdout
    UploadScan (DestinationMeta (apiOpts, projectMetadata)) -> doUpload apiOpts projectMetadata
    OutputAndUpload (DestinationMeta (apiOpts, projectMetadata)) -> scannedImageToStdout >> doUpload apiOpts projectMetadata

  pure scannedImage

uploadScan ::
  ( Has Diagnostics sig m
  , Has FossaApiClient sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  ) =>
  Organization ->
  ProjectRevision ->
  ProjectMetadata ->
  Flag JsonOutput ->
  ContainerScan ->
  m Locator
uploadScan orgInfo revision projectMeta jsonOutput containerScan =
  do
    let supportsNativeScan = orgSupportsNativeContainerScan orgInfo
    if not supportsNativeScan
      then fatal (EndpointDoesNotSupportNativeContainerScan getSourceLocation)
      else do
        void $ applyReleaseGroupDeprecationWarning projectMeta
        resp <- uploadNativeContainerScan revision projectMeta containerScan
        emitBuildWarnings resp
        let locator = uploadLocator resp
        buildUrl <- getFossaBuildUrl revision locator

        logInfo "View FOSSA Report:"
        logInfo ("  " <> pretty buildUrl)
        traverse_ (\err -> logError $ "FOSSA error: " <> viaShow err) (uploadError resp)

        when (fromFlag JsonOutput jsonOutput) $ do
          summary <-
            context "Analysis upload succeeded" $
              buildJsonSummary revision locator buildUrl
          logStdout . decodeUtf8 $ Aeson.encode summary
        -- We return locator for purely for testing.
        pure locator

buildJsonSummary :: (Has Diagnostics sig m) => ProjectRevision -> Locator -> Text -> m Aeson.Value
buildJsonSummary project locator projectUrl = do
  revision <- fromMaybeText "Server returned an invalid project revision" $ locatorRevision locator
  pure $
    Aeson.object
      [ "project" .= locatorProject locator
      , "revision" .= revision
      , "branch" .= projectBranch project
      , "url" .= projectUrl
      , "id" .= renderLocator locator
      ]
