module App.Fossa.SBOM.Analyze (
  analyze,
) where

import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Config.SBOM
import App.Fossa.Config.SBOM.Analyze (SBOMScanDestination (..))
import App.Fossa.PreflightChecks (PreflightCommandChecks (..), preflightChecks)
import App.Fossa.ProjectInference (InferredProject (..), inferProjectDefaultFromFile)
import App.Types (ComponentUploadFileType (..), OverrideProject (..), ProjectMetadata (..), ProjectRevision (..))
import Control.Carrier.Debug (Debug)
import Control.Carrier.Diagnostics (fromEitherShow)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.StickyLogger (StickyLogger, logSticky, runStickyLogger)
import Control.Effect.Diagnostics (context)
import Control.Effect.FossaApiClient (FossaApiClient, PackageRevision (PackageRevision), getOrganization, getSignedUploadUrl, queueSBOMBuild, uploadArchive)
import Control.Effect.Lift
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.String.Conversion
import Data.Text (Text)
import Effect.Logger (Logger, logDebug, logInfo)
import Fossa.API.Types
import Path (parseSomeFile)
import Path.Posix (SomeBase (..))
import Prettyprinter (Pretty (pretty))
import Srclib.Types (Locator (..))

uploadSBOM ::
  ( Has StickyLogger sig m
  , Has FossaApiClient sig m
  , Has Logger sig m
  ) =>
  SBOMAnalyzeConfig ->
  ProjectRevision ->
  m ()
uploadSBOM conf revision = do
  signedURL <- getSignedUploadUrl SBOMUpload $ PackageRevision (projectName revision) (projectRevision revision)
  let path = unSBOMFile $ sbomPath conf

  logSticky $ "Uploading '" <> (projectName revision) <> "' to secure S3 bucket"
  res <- uploadArchive signedURL $ toString path
  logDebug $ pretty $ show res

  pure ()

getProjectRevision ::
  ( Has Diag.Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  SBOMAnalyzeConfig ->
  m ProjectRevision
getProjectRevision conf = do
  let path = unSBOMFile $ sbomPath conf
  parsedPath <- context "Parsing `sbom` path" $ fromEitherShow $ parseSomeFile (toString path)
  inferred <- case parsedPath of
    Abs f -> inferProjectDefaultFromFile f
    Rel f -> inferProjectDefaultFromFile f

  let name = fromMaybe (inferredName inferred) $ overrideName (revisionOverride conf)
  let version = fromMaybe (inferredRevision inferred) $ overrideRevision (revisionOverride conf)
  pure $ ProjectRevision name version Nothing

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
  revision <- getProjectRevision config
  _ <- case sbomScanDestination config of
    SBOMOutputStdout -> pure ()
    SBOMUploadScan apiOpts -> runFossaApiClient apiOpts $ preflightChecks $ AnalyzeChecks revision emptyMetadata
  case sbomScanDestination config of
    SBOMOutputStdout -> pure ()
    SBOMUploadScan apiOpts -> (runFossaApiClient apiOpts) . runStickyLogger (severity config) $ analyzeInternal config revision

analyzeInternal ::
  ( Has Diag.Diagnostics sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has FossaApiClient sig m
  ) =>
  SBOMAnalyzeConfig ->
  ProjectRevision ->
  m ()
analyzeInternal config revision = do
  -- First, upload the SBOM to S3
  _ <- uploadSBOM config revision

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
