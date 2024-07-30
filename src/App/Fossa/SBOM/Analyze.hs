module App.Fossa.SBOM.Analyze (
  analyze,
  analyzeInternal,
) where

import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Config.SBOM
import App.Fossa.PreflightChecks (PreflightCommandChecks (..), preflightChecks)
import App.Types (ComponentUploadFileType (..), ProjectMetadata (..), ProjectRevision (..))
import Control.Carrier.Debug (Debug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.StickyLogger (StickyLogger, logSticky, runStickyLogger)
import Control.Carrier.Telemetry.Types (CountableCliFeature (SBOMAnalyzeUsage))
import Control.Effect.FossaApiClient (FossaApiClient, PackageRevision (PackageRevision), getOrganization, getSignedUploadUrl, queueSBOMBuild, uploadArchive)
import Control.Effect.Lift
import Control.Effect.Telemetry (Telemetry, trackUsage)
import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.String.Conversion (ConvertUtf8 (..), toString, toText)
import Data.Text (Text)
import Effect.Logger (Logger, logDebug, logInfo)
import Fossa.API.Types
import Prettyprinter (Pretty (pretty))
import Srclib.Types (Locator (..))

analyze ::
  ( Has Diag.Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has Debug sig m
  , Has Telemetry sig m
  ) =>
  SBOMAnalyzeConfig ->
  m ()
analyze config = do
  let emptyMetadata = ProjectMetadata Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing
  let apiOpts = sbomApiOpts config
  trackUsage SBOMAnalyzeUsage
  void . runFossaApiClient apiOpts . preflightChecks $ AnalyzeChecks (sbomRevision config) emptyMetadata
  runFossaApiClient apiOpts . runStickyLogger (severity config) $ analyzeInternal config

analyzeInternal ::
  ( Has Diag.Diagnostics sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has FossaApiClient sig m
  ) =>
  SBOMAnalyzeConfig ->
  m ()
analyzeInternal config = do
  let revision = sbomRevision config
  void $ uploadSBOM config

  let archive = Archive (projectName revision) (projectRevision revision) Nothing Nothing
  _ <- queueSBOMBuild archive (sbomTeam config) (sbomRebuild config)

  -- The locator used in the URL has the organization ID on it, so we
  -- need to generate that locator before displaying it
  orgId <- organizationId <$> getOrganization
  let updateRevisionName :: Text -> ProjectRevision -> ProjectRevision
      updateRevisionName updateText r = r{projectName = updateText <> "/" <> projectName r}
      revisionWithOrganization = updateRevisionName (toText $ show orgId) revision
      locator = Locator "sbom" (projectName revisionWithOrganization) (Just $ projectRevision revision)
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

uploadSBOM ::
  ( Has StickyLogger sig m
  , Has FossaApiClient sig m
  , Has Logger sig m
  ) =>
  SBOMAnalyzeConfig ->
  m ()
uploadSBOM config = do
  let revision = sbomRevision config
  signedURL <- getSignedUploadUrl SBOMUpload $ PackageRevision (projectName revision) (projectRevision revision)
  let path = unSBOMFile $ sbomPath config

  logSticky $ "Uploading '" <> (projectName revision) <> "' to secure S3 bucket"
  res <- uploadArchive signedURL $ toString path
  logDebug . pretty $ (decodeUtf8 @Text res)

  pure ()
