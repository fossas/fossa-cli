module App.Fossa.SBOM.Analyze (
  analyze,
) where

import App.Fossa.Config.Analyze (ScanDestination (..))
import App.Fossa.Config.SBOM
import App.Fossa.ProjectInference (InferredProject (..), inferProjectDefaultFromFile)
import App.Types (ComponentUploadFileType (..), OverrideProject (..))
import Control.Carrier.Debug (Debug)
import Control.Carrier.Diagnostics (Diagnostics, fromEitherShow)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.FossaApiClient (runFossaApiClient)
import Control.Carrier.StickyLogger (StickyLogger, logSticky, runStickyLogger)
import Control.Effect.Diagnostics (context)
import Control.Effect.FossaApiClient (FossaApiClient, PackageRevision (PackageRevision), getOrganization, getSignedUploadUrl, queueSBOMBuild, uploadArchive)
import Control.Effect.Lift
import Data.Maybe (fromMaybe)
import Data.String.Conversion
import Data.Text (Text)
import Effect.Logger (Logger, logDebug)
import Fossa.API.Types
import Path (parseSomeFile)
import Path.Posix (SomeBase (..))
import Prettyprinter (Pretty (pretty))
import Srclib.Types (Locator (..))

data SBOM = SBOM
  { sbomName :: Text
  , sbomVersion :: Text
  }
  deriving (Eq, Ord, Show)

uploadSBOM ::
  ( Has StickyLogger sig m
  , Has FossaApiClient sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , Has Diagnostics sig m
  ) =>
  SBOMAnalyzeConfig ->
  m SBOM
uploadSBOM conf = do
  -- let sbomFile = sbomPath conf
  let path = unSBOMFile $ sbomPath conf
  parsedPath <- context "Parsing `sbom` path" $ fromEitherShow $ parseSomeFile (toString path)
  inferred <- case parsedPath of
    Abs f -> inferProjectDefaultFromFile f
    Rel f -> inferProjectDefaultFromFile f

  let depVersion = fromMaybe (inferredRevision inferred) $ overrideRevision (revisionOverride conf)
  let vendoredName = fromMaybe (inferredName inferred) $ overrideName (revisionOverride conf)
  -- TODO: The version from the UI is a timestamp. Should we keep that or use the hash?
  -- depVersion <- case vendoredVersion of
  --   Nothing -> sendIO $ hashFile compressedFile
  --   Just version -> pure version

  signedURL <- getSignedUploadUrl SBOMUpload $ PackageRevision vendoredName depVersion

  logSticky $ "Uploading '" <> vendoredName <> "' to secure S3 bucket"
  res <- uploadArchive signedURL $ toString path
  logDebug $ pretty $ show res

  pure $ SBOM vendoredName depVersion

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
analyze config =
  case sbomScanDestination config of
    OutputStdout -> pure ()
    UploadScan apiOpts _metadata -> (runFossaApiClient apiOpts) . runStickyLogger (severity config) $ analyzeInternal config

analyzeInternal ::
  ( Has Diag.Diagnostics sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has FossaApiClient sig m
  , Has (Lift IO) sig m
  ) =>
  SBOMAnalyzeConfig ->
  m ()
analyzeInternal config = do
  -- At this point, we have a good list of deps, so go for it.
  sbom <- uploadSBOM config

  -- queueArchiveBuild takes archives without Organization information. This
  -- orgID is appended when creating the build on the backend.  We don't care
  -- about the response here because if the build has already been queued, we
  -- get a 401 response.

  let archive = Archive (sbomName sbom) (sbomVersion sbom)
  _ <- queueSBOMBuild archive (sbomRebuild config)
  -- The organizationID is needed to prefix each locator name. The FOSSA API
  -- automatically prefixes the locator when queuing the build but not when
  -- reading from a source unit.
  orgId <- organizationId <$> getOrganization

  let updateSBOMName :: Text -> SBOM -> SBOM
      updateSBOMName updateText s = s{sbomName = updateText <> "/" <> sbomName s}
      sbomWithOrganization = updateSBOMName (toText $ show orgId) sbom

  let locator = Locator "sbom" (sbomName sbomWithOrganization) (Just $ sbomVersion sbom)
  logSticky $ "uploaded to " <> toText locator
  pure ()
