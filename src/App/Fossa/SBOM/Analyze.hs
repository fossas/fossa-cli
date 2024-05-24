{-# LANGUAGE RecordWildCards #-}

module App.Fossa.SBOM.Analyze (
  analyze,
) where

import App.Fossa.Config.SBOM
import App.Fossa.VendoredDependency (
  compressFile,
 )
import App.Types (BaseDir (unBaseDir), DependencyRebuild)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.StickyLogger (StickyLogger, logSticky)
import Control.Effect.Diagnostics (context)
import Control.Effect.FossaApiClient (FossaApiClient, PackageRevision (PackageRevision), getOrganization, getSignedUploadUrl, queueArchiveBuild, uploadArchive)
import Control.Effect.Lift
import Control.Effect.Path (withSystemTempDir)
import Data.String.Conversion
import Data.Text (Text)
import Effect.Logger (Logger, logDebug)
import Fossa.API.Types
import Path hiding ((</>))
import Prettyprinter (Pretty (pretty))
import Srclib.Types (Locator (..))

data SBOM = SBOM
  { sbomName :: Text
  , sbomVersion :: Text
  }
  deriving (Eq, Ord, Show)

uploadSBOM ::
  ( Has Diag.Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has FossaApiClient sig m
  ) =>
  SBOMAnalyzeConfig ->
  Path Abs Dir ->
  m SBOM
uploadSBOM conf tmpDir = context "compressing and uploading SBOM" $ do
  let sbomFile = sbomPath conf
  let baseDir = unBaseDir $ sbomBaseDir conf
  let sbomPath = unSBOMFile sbomFile
  logSticky $ "Compressing '" <> (toText sbomPath) <> "'"
  compressedFile <- sendIO $ compressFile tmpDir baseDir (toString sbomPath)

  let depVersion = "1.2.3"
  let vendoredName = "someSbom"
  -- TODO: The version from the UI is a timestamp. Should we keep that or use the hash?
  -- depVersion <- case vendoredVersion of
  --   Nothing -> sendIO $ hashFile compressedFile
  --   Just version -> pure version

  signedURL <- getSignedUploadUrl $ PackageRevision vendoredName depVersion

  logSticky $ "Uploading '" <> vendoredName <> "' to secure S3 bucket"
  res <- uploadArchive signedURL compressedFile
  logDebug $ pretty $ show res

  pure $ SBOM vendoredName depVersion

-- archiveUploadSourceUnit receives a list of vendored dependencies, a root path, and API settings.
-- Using this information, it uploads each vendored dependency and queues a build for the dependency.
--
-- Note: this function intentionally does not accept a @FileUpload@ type, because it /always/ uploads full files.
analyze ::
  ( Has Diag.Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has FossaApiClient sig m
  ) =>
  SBOMAnalyzeConfig ->
  m Locator
analyze config = do
  -- At this point, we have a good list of deps, so go for it.
  sbom <- withSystemTempDir "fossa-temp" (uploadSBOM config)

  -- queueArchiveBuild takes archives without Organization information. This
  -- orgID is appended when creating the build on the backend.  We don't care
  -- about the response here because if the build has already been queued, we
  -- get a 401 response.

  -- TODO
  -- _ <- queueSBOMBuild sbom rebuild

  -- The organizationID is needed to prefix each locator name. The FOSSA API
  -- automatically prefixes the locator when queuing the build but not when
  -- reading from a source unit.
  orgId <- organizationId <$> getOrganization

  let updateSBOMName :: Text -> SBOM -> SBOM
      updateSBOMName updateText s = s{sbomName = updateText <> "/" <> sbomName s}
      sbomWithOrganization = updateSBOMName (toText $ show orgId) sbom

  pure $ Locator "sbom" (sbomName sbomWithOrganization) (Just $ sbomVersion sbom)
