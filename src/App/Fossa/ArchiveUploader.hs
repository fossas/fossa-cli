{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ArchiveUploader
  ( archiveUploadSourceUnit,
    arcToLocator,
    compressFile,
    forceVendoredToArchive,
    duplicateFailureBundle,
    duplicateNames,
    hashFile,
    VendoredDependency (..),
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.StickyLogger (StickyLogger, logSticky)
import Control.Effect.Diagnostics (context)
import Control.Effect.FossaApiClient (FossaApiClient, PackageRevision (PackageRevision), getOrganization, getSignedUploadUrl, queueArchiveBuild, uploadArchive)
import Control.Effect.Lift
import Control.Effect.Path (withSystemTempDir)
import Control.Monad (unless)
import Crypto.Hash
import Data.Aeson
  ( FromJSON (parseJSON),
    withObject,
    (.:),
    (.:?),
  )
import Data.Aeson.Extra
import Data.ByteString.Lazy qualified as BS
import Data.Functor.Extra ((<$$>))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.String.Conversion
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID.V4 (nextRandom)
import Effect.Logger (Logger, logDebug)
import Fossa.API.Types
import Path hiding ((</>))
import Prettyprinter (Pretty (pretty))
import Srclib.Types (Locator (..))
import System.FilePath.Posix

data VendoredDependency = VendoredDependency
  { vendoredName :: Text,
    vendoredPath :: Text,
    vendoredVersion :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON VendoredDependency where
  parseJSON = withObject "VendoredDependency" $ \obj ->
    VendoredDependency <$> obj .: "name"
      <*> obj .: "path"
      <*> (unTextLike <$$> obj .:? "version")
      <* forbidMembers "vendored dependencies" ["type", "license", "url", "description"] obj

uploadArchives ::
  ( Has Diag.Diagnostics sig m,
    Has (Lift IO) sig m,
    Has StickyLogger sig m,
    Has Logger sig m,
    Has FossaApiClient sig m
  ) =>
  NonEmpty VendoredDependency ->
  Path Abs Dir ->
  Path Abs Dir ->
  m (NonEmpty Archive)
uploadArchives deps arcDir tmpDir = traverse (compressAndUpload arcDir tmpDir) deps

compressAndUpload ::
  ( Has Diag.Diagnostics sig m,
    Has (Lift IO) sig m,
    Has StickyLogger sig m,
    Has Logger sig m,
    Has FossaApiClient sig m
  ) =>
  Path Abs Dir ->
  Path Abs Dir ->
  VendoredDependency ->
  m Archive
compressAndUpload arcDir tmpDir VendoredDependency {..} = context "compressing and uploading vendored deps" $ do
  logSticky $ "Compressing '" <> vendoredName <> "' at '" <> vendoredPath <> "'"
  compressedFile <- sendIO $ compressFile tmpDir arcDir (toString vendoredPath)

  depVersion <- case vendoredVersion of
    Nothing -> sendIO $ hashFile compressedFile
    Just version -> pure version

  signedURL <- getSignedUploadUrl $ PackageRevision vendoredName depVersion

  logSticky $ "Uploading '" <> vendoredName <> "' to secure S3 bucket"
  res <- uploadArchive signedURL compressedFile
  logDebug $ pretty $ show res

  pure $ Archive vendoredName depVersion

-- archiveUploadSourceUnit receives a list of vendored dependencies, a root path, and API settings.
-- Using this information, it uploads each vendored dependency and queues a build for the dependency.
archiveUploadSourceUnit ::
  ( Has Diag.Diagnostics sig m,
    Has (Lift IO) sig m,
    Has StickyLogger sig m,
    Has Logger sig m,
    Has FossaApiClient sig m
  ) =>
  Path Abs Dir ->
  NonEmpty VendoredDependency ->
  m (NonEmpty Locator)
archiveUploadSourceUnit baseDir vendoredDeps = do
  -- Users with many instances of vendored dependencies may accidentally have complete duplicates. Remove them.
  let uniqDeps = NonEmpty.nub vendoredDeps

  -- However, users may also have vendored dependencies that have duplicate names but are not complete duplicates.
  -- These aren't valid and can't be automatically handled, so fail the scan with them.
  let duplicates = duplicateNames uniqDeps
  unless (null duplicates) $ Diag.fatalText $ duplicateFailureBundle duplicates

  -- At this point, we have a good list of deps, so go for it.
  archives <- withSystemTempDir "fossa-temp" (uploadArchives uniqDeps baseDir)

  -- queueArchiveBuild takes archives without Organization information. This
  -- orgID is appended when creating the build on the backend.  We don't care
  -- about the response here because if the build has already been queued, we
  -- get a 401 response.
  res <- traverse queueArchiveBuild (NonEmpty.toList archives)
  logDebug $ pretty $ show res

  -- The organizationID is needed to prefix each locator name. The FOSSA API
  -- automatically prefixes the locator when queuing the build but not when
  -- reading from a source unit.
  orgId <- organizationId <$> getOrganization

  let updateArcName :: Text -> Archive -> Archive
      updateArcName updateText arc = arc {archiveName = updateText <> "/" <> archiveName arc}
      archivesWithOrganization = updateArcName (toText $ show orgId) <$> archives

  pure $ arcToLocator <$> archivesWithOrganization

-- | List of names that occur more than once in a list of vendored dependencies.
duplicateNames :: NonEmpty VendoredDependency -> [Text]
duplicateNames = Map.keys . Map.filter (> 1) . Map.fromListWith (+) . map pair . NonEmpty.toList
  where
    pair :: VendoredDependency -> (Text, Int)
    pair VendoredDependency {vendoredName} = (vendoredName, 1)

duplicateFailureBundle :: [Text] -> Text
duplicateFailureBundle names =
  "The provided vendored dependencies contain the following duplicate names:\n\t"
    <> Text.intercalate "\n\t" names
    <> "\n\n"
    <> "Vendored dependency entries may not specify duplicate names.\n"
    <> "Please ensure that each vendored dependency entry has a unique name."

forceVendoredToArchive :: VendoredDependency -> Archive
forceVendoredToArchive dep = Archive (vendoredName dep) (fromMaybe "" $ vendoredVersion dep)

arcToLocator :: Archive -> Locator
arcToLocator arc =
  Locator
    { locatorFetcher = "archive",
      locatorProject = archiveName arc,
      locatorRevision = Just $ archiveVersion arc
    }

compressFile :: Path Abs Dir -> Path Abs Dir -> FilePath -> IO FilePath
compressFile outputDir directory fileToTar = do
  -- We are adding the suffix to avoid errors when we compress to a path that already exists
  -- This is most likely to happen if `fileToTar` is "."
  suffix <- nextRandom
  let finalFilename = fileToTar ++ show suffix
  let finalFile = toString outputDir </> safeSeparators finalFilename
  entries <- Tar.pack (toString directory) [fileToTar]
  BS.writeFile finalFile $ GZip.compress $ Tar.write entries
  pure finalFile

md5 :: BS.ByteString -> Digest MD5
md5 = hashlazy

hashFile :: FilePath -> IO Text
hashFile fileToHash = do
  fileContent <- BS.readFile fileToHash
  pure . toText . show $ md5 fileContent

safeSeparators :: FilePath -> FilePath
safeSeparators = intercalate "_" . splitDirectories
