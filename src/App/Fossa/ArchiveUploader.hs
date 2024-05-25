{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ArchiveUploader (
  archiveUploadSourceUnit,
) where

import App.Fossa.VendoredDependency (
  VendoredDependency (..),
  arcToLocator,
  compressFile,
  dedupVendoredDeps,
  hashFile,
 )
import App.Types (ComponentUploadFileType (..), DependencyRebuild)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.StickyLogger (StickyLogger, logSticky)
import Control.Effect.Diagnostics (context)
import Control.Effect.FossaApiClient (FossaApiClient, PackageRevision (PackageRevision), getOrganization, getSignedUploadUrl, queueArchiveBuild, uploadArchive)
import Control.Effect.Lift
import Control.Effect.Path (withSystemTempDir)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.String.Conversion
import Data.Text (Text)
import Effect.Logger (Logger, logDebug)
import Fossa.API.Types
import Path hiding ((</>))
import Prettyprinter (Pretty (pretty))
import Srclib.Types (Locator (..))

uploadArchives ::
  ( Has Diag.Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has FossaApiClient sig m
  ) =>
  NonEmpty VendoredDependency ->
  Path Abs Dir ->
  Path Abs Dir ->
  m (NonEmpty Archive)
uploadArchives deps arcDir tmpDir = traverse (compressAndUpload arcDir tmpDir) deps

compressAndUpload ::
  ( Has Diag.Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has FossaApiClient sig m
  ) =>
  Path Abs Dir ->
  Path Abs Dir ->
  VendoredDependency ->
  m Archive
compressAndUpload arcDir tmpDir VendoredDependency{..} = context "compressing and uploading vendored deps" $ do
  logSticky $ "Compressing '" <> vendoredName <> "' at '" <> vendoredPath <> "'"
  compressedFile <- sendIO $ compressFile tmpDir arcDir (toString vendoredPath)

  depVersion <- case vendoredVersion of
    Nothing -> sendIO $ hashFile compressedFile
    Just version -> pure version

  signedURL <- getSignedUploadUrl ArchiveUpload $ PackageRevision vendoredName depVersion

  logSticky $ "Uploading '" <> vendoredName <> "' to secure S3 bucket"
  res <- uploadArchive signedURL compressedFile
  logDebug $ pretty $ show res

  pure $ Archive vendoredName depVersion

-- archiveUploadSourceUnit receives a list of vendored dependencies, a root path, and API settings.
-- Using this information, it uploads each vendored dependency and queues a build for the dependency.
--
-- Note: this function intentionally does not accept a @FileUpload@ type, because it /always/ uploads full files.
archiveUploadSourceUnit ::
  ( Has Diag.Diagnostics sig m
  , Has (Lift IO) sig m
  , Has StickyLogger sig m
  , Has Logger sig m
  , Has FossaApiClient sig m
  ) =>
  DependencyRebuild ->
  Path Abs Dir ->
  NonEmpty VendoredDependency ->
  m (NonEmpty Locator)
archiveUploadSourceUnit rebuild baseDir vendoredDeps = do
  uniqDeps <- dedupVendoredDeps vendoredDeps

  -- At this point, we have a good list of deps, so go for it.
  archives <- withSystemTempDir "fossa-temp" (uploadArchives uniqDeps baseDir)

  -- queueArchiveBuild takes archives without Organization information. This
  -- orgID is appended when creating the build on the backend.  We don't care
  -- about the response here because if the build has already been queued, we
  -- get a 401 response.
  _ <- queueArchiveBuild (NonEmpty.toList archives) rebuild

  -- The organizationID is needed to prefix each locator name. The FOSSA API
  -- automatically prefixes the locator when queuing the build but not when
  -- reading from a source unit.
  orgId <- organizationId <$> getOrganization

  let updateArcName :: Text -> Archive -> Archive
      updateArcName updateText arc = arc{archiveName = updateText <> "/" <> archiveName arc}
      archivesWithOrganization = updateArcName (toText $ show orgId) <$> archives

  pure $ arcToLocator <$> archivesWithOrganization
