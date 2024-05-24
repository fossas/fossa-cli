module App.Fossa.SBOM.Analyze (
  SBOMImageSource (..),
  analyzeExperimental,

  -- * for testing
  uploadScan,
) where

import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Analyze.Debug (collectDebugBundle)
import App.Fossa.Analyze.Upload (emitBuildWarnings)
import App.Fossa.Config.Common (
  ScanDestination (OutputStdout, UploadScan),
  deprecateReleaseGroupMetadata,
 )
import App.Fossa.Config.SBOM.Analyze (
  JsonOutput (JsonOutput),
  SBOMAnalyzeConfig (..),
 )
import App.Fossa.Config.SBOM.Analyze qualified as Config
import App.Fossa.PreflightChecks (PreflightCommandChecks (AnalyzeChecks), preflightChecks)
import App.Types (
  ProjectMetadata,
  ProjectRevision (..),
 )
import Codec.Compression.GZip qualified as GZip
import Control.Carrier.Debug (Debug, ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Effect.Diagnostics (Diagnostics, context, fatal, fromMaybeText)
import Control.Effect.FossaApiClient (FossaApiClient, getOrganization)
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
import Fossa.API.Types (Organization (), UploadResponse (uploadError), uploadLocator)
import Path (Abs, File, Path)
import Srclib.Types (Locator (locatorRevision), locatorProject, renderLocator)

debugBundlePath :: FilePath
debugBundlePath = "fossa.debug.json.gz"

analyze ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Telemetry sig m
  , Has Debug sig m
  ) =>
  SBOMAnalyzeConfig ->
  m SBOMScan
analyze cfg = do
  scannedImage <- scanImage (filterSet cfg) (withoutDefaultFilters cfg) (onlySystemDeps cfg) (imageLocator cfg) (dockerHost cfg) (arch cfg)
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
  SBOMScan ->
  m Locator
uploadScan revision projectMeta sbomScan =
  do
    supportsNativeScan <- orgSupportsNativeSBOMScan <$> getOrganization
    if not supportsNativeScan
      then fatal (EndpointDoesNotSupportNativeSBOMScan getSourceLocation)
      else do
        projectMetadataWithoutReleaseGroup <- deprecateReleaseGroupMetadata projectMeta
        resp <- uploadNativeSBOMScan revision projectMetadataWithoutReleaseGroup sbomScan
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
