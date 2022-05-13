module App.Fossa.VendoredDependency (
  VendoredDependency (..),
  arcToLocator,
  forceVendoredToArchive,
  compressFile,
  duplicateFailureBundle,
  duplicateNames,
  hashFile,
) where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Crypto.Hash (Digest, MD5, hashlazy)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Aeson.Extra (TextLike (unTextLike), forbidMembers)
import Data.ByteString.Lazy qualified as BS
import Data.Functor.Extra ((<$$>))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.String.Conversion (
  ToString (toString),
  ToText (toText),
 )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID.V4 (nextRandom)
import Fossa.API.Types (Archive (..))
import Path.Posix (Abs, Dir, Path)
import Srclib.Types (Locator (..))
import System.FilePath.Posix (splitDirectories, (</>))

data VendoredDependency = VendoredDependency
  { vendoredName :: Text
  , vendoredPath :: Text
  , vendoredVersion :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON VendoredDependency where
  parseJSON = withObject "VendoredDependency" $ \obj ->
    VendoredDependency <$> obj .: "name"
      <*> obj .: "path"
      <*> (unTextLike <$$> obj .:? "version")
      <* forbidMembers "vendored dependencies" ["type", "license", "url", "description"] obj

-- | List of names that occur more than once in a list of vendored dependencies.
duplicateNames :: NonEmpty VendoredDependency -> [Text]
duplicateNames = Map.keys . Map.filter (> 1) . Map.fromListWith (+) . map pair . NonEmpty.toList
  where
    pair :: VendoredDependency -> (Text, Int)
    pair VendoredDependency{vendoredName} = (vendoredName, 1)

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
    { locatorFetcher = "archive"
    , locatorProject = archiveName arc
    , locatorRevision = Just $ archiveVersion arc
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
