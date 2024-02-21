module App.Fossa.Container.AnalyzeNative (
  ContainerImageSource (..),
  analyzeExperimental,

  -- * for testing
  uploadScan,
) where

import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Analyze.Debug (collectDebugBundle)
import App.Fossa.Config.Common (
  ScanDestination (OutputStdout, UploadScan),
 )
import App.Fossa.Config.Container.Analyze (
  ContainerAnalyzeConfig (..),
  JsonOutput (JsonOutput),
 )
import App.Fossa.Config.Container.Analyze qualified as Config
import App.Fossa.Container.Scan (extractRevision, scanImage)
import App.Fossa.PreflightChecks (PreflightCommandChecks (AnalyzeChecks), preflightChecks)
import App.Types (
  ProjectMetadata,
  ProjectRevision (..),
 )
import Codec.Compression.GZip qualified as GZip
import Container.Docker.SourceParser (RegistryImageSource)
import Container.Errors (EndpointDoesNotSupportNativeContainerScan (EndpointDoesNotSupportNativeContainerScan))
import Container.Types (ContainerScan (..))
import Control.Carrier.Debug (Debug, ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Effect.Diagnostics (Diagnostics, context, fatal, fromMaybeText)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization, uploadNativeContainerScan)
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
  Severity (..),
  logError,
  logInfo,
  logStdout,
  viaShow,
 )
import Effect.ReadFS (ReadFS)
import Fossa.API.Types (Organization (orgSupportsNativeContainerScan), UploadResponse (uploadError), uploadLocator)
import Path (Abs, File, Path)
import Srclib.Types (Locator (locatorRevision), locatorProject, renderLocator)

data ContainerImageSource
  = DockerArchive (Path Abs File)
  | DockerEngine Text
  | Podman Text
  | Registry RegistryImageSource
  deriving (Show, Eq)

debugBundlePath :: FilePath
debugBundlePath = "fossa.debug.json.gz"

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
  case Config.severity cfg of
    SevDebug -> do
      (scope, res) <- collectDebugBundle cfg $ Diag.errorBoundaryIO $ analyze cfg
      sendIO . BL.writeFile debugBundlePath . GZip.compress $ Aeson.encode scope
      Diag.rethrow res
    _ -> ignoreDebug $ analyze cfg

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
  scannedImage <- scanImage (filterSet cfg) (onlySystemDeps cfg) (imageLocator cfg) (dockerHost cfg) (arch cfg)
  let revision = extractRevision (revisionOverride cfg) scannedImage

  _ <- case scanDestination cfg of
    OutputStdout -> pure ()
    UploadScan apiOpts projectMetadata -> runFossaApiClient apiOpts $ preflightChecks $ AnalyzeChecks revision projectMetadata

  logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
  logInfo ("Using project revision: `" <> pretty (projectRevision revision) <> "`")

  let branchText = fromMaybe "No branch (detached HEAD)" $ projectBranch revision
  logInfo ("Using branch: `" <> pretty branchText <> "`")

  case scanDestination cfg of
    OutputStdout -> logStdout . decodeUtf8 $ Aeson.encode scannedImage
    UploadScan apiOpts projectMeta ->
      void $ runFossaApiClient apiOpts $ uploadScan revision projectMeta (jsonOutput cfg) scannedImage

  pure scannedImage

uploadScan ::
  ( Has Diagnostics sig m
  , Has FossaApiClient sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  ) =>
  ProjectRevision ->
  ProjectMetadata ->
  Flag JsonOutput ->
  ContainerScan ->
  m Locator
uploadScan revision projectMeta jsonOutput containerScan =
  do
    supportsNativeScan <- orgSupportsNativeContainerScan <$> getOrganization
    if not supportsNativeScan
      then fatal (EndpointDoesNotSupportNativeContainerScan getSourceLocation)
      else do
        resp <- uploadNativeContainerScan revision projectMeta containerScan
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
