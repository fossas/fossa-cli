module App.Fossa.SBOM.Analyze (
  analyze,
) where

import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Config.SBOM
import App.Fossa.Config.SBOM.Analyze (SBOMScanDestination (..))
import App.Fossa.PreflightChecks (PreflightCommandChecks (..), preflightChecks)
import App.Types (ComponentUploadFileType (..), ProjectMetadata (..), ProjectRevision (..))
import Control.Carrier.Debug (Debug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.StickyLogger (StickyLogger, logSticky, runStickyLogger)
import Control.Effect.FossaApiClient (FossaApiClient, PackageRevision (PackageRevision), getOrganization, getSignedUploadUrl, queueSBOMBuild, uploadArchive)
import Control.Effect.Lift
import Data.Foldable (traverse_)
import Data.String.Conversion
import Data.Text (Text)
import Effect.Logger (Logger, logDebug, logInfo)
import Fossa.API.Types
import Prettyprinter (Pretty (pretty))
import Srclib.Types (Locator (..))

uploadSBOM ::
  ( Has StickyLogger sig m
  , Has FossaApiClient sig m
  , Has Logger sig m
  ) =>
  SBOMAnalyzeConfig ->
  m ()
uploadSBOM conf = do
  let revision = sbomRevision conf
  signedURL <- getSignedUploadUrl SBOMUpload $ PackageRevision (projectName revision) (projectRevision revision)
  let path = unSBOMFile $ sbomPath conf

  logSticky $ "Uploading '" <> (projectName revision) <> "' to secure S3 bucket"
  res <- uploadArchive signedURL $ toString path
  logDebug $ pretty $ show res

  pure ()

-- analyze receives a path to an SBOM file, a root path, and API settings.
-- Using this information, it uploads the SBOM and queues a build for it.
analyze ::
  ( Has Diag.Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has Debug sig m
  ) =>
  SBOMAnalyzeConfig ->
  m ()
analyze config = do
  let emptyMetadata = ProjectMetadata Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing
  _ <- case sbomScanDestination config of
    SBOMOutputStdout -> pure ()
    SBOMUploadScan apiOpts -> runFossaApiClient apiOpts $ preflightChecks $ AnalyzeChecks (sbomRevision config) emptyMetadata
  case sbomScanDestination config of
    SBOMOutputStdout -> pure ()
    SBOMUploadScan apiOpts -> (runFossaApiClient apiOpts) . runStickyLogger (severity config) $ analyzeInternal config
analyzeInternal ::
  ( Has Diag.Diagnostics sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has FossaApiClient sig m
  ) =>
  SBOMAnalyzeConfig ->
  m ()
analyzeInternal config = do
  -- First, upload the SBOM to S3
  let revision = sbomRevision config
  _ <- uploadSBOM config
  -- Second, trigger a build
  let archive = Archive (projectName revision) (projectRevision revision)
  _ <- queueSBOMBuild archive (sbomTeam config) (sbomRebuild config)

  -- The organizationID is needed to prefix each locator name. The FOSSA API
  -- automatically prefixes the locator when queuing the build but not when
  -- reading from a source unit.
  orgId <- organizationId <$> getOrganization
  let updateRevisionName :: Text -> ProjectRevision -> ProjectRevision
      updateRevisionName updateText r = r{projectName = updateText <> "/" <> projectName r}
      revisionWithOrganization = updateRevisionName (toText $ show orgId) revision

  let locator = Locator "sbom" (projectName revisionWithOrganization) (Just $ projectRevision revision)
  logSticky $ "uploaded to " <> toText locator
  buildUrl <- getFossaBuildUrl revisionWithOrganization locator

  traverse_
    logInfo
    [ "============================================================"
    , ""
    , "    View FOSSA Report:"
    , "    " <> pretty buildUrl
    , ""
    , "============================================================"
    ]
