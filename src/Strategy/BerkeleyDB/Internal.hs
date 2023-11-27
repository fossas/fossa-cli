module Strategy.BerkeleyDB.Internal (
  readBerkeleyDB,
  BdbEntry (..),
) where

import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withBerkeleyBinary)
import Control.Effect.Diagnostics (Diagnostics, context, fatalOnSomeException, fromEitherShow)
import Control.Effect.Lift (Lift, sendIO)
import Control.Monad (when)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (catMaybes)
import Data.Rpm.DbHeaderBlob (PkgInfo (..), readPackageInfo)
import Data.String.Conversion (ConvertUtf8 (encodeUtf8), decodeUtf8, toText)
import Data.Text (Text)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execJson')
import Effect.Logger (Logger, Pretty (pretty), logDebug, logWarn)
import Effect.ReadFS (Has, ReadFS, readContentsBS)
import Path (Abs, Dir, File, Path, parseAbsDir)
import System.Directory (getCurrentDirectory)

-- | An entry in the database, consisting of the architecture, package, and version.
data BdbEntry = BdbEntry
  { bdbEntryArch :: Text
  , bdbEntryPackage :: Text
  , bdbEntryVersion :: Text
  , bdbEntryEpoch :: Maybe Text
  }
  deriving (Eq, Ord, Show)

-- | FOSSA _requires_ that architecture is provided: https://github.com/fossas/FOSSA/blob/e61713dec1ef80dc6b6114f79622c14df5278235/modules/fetchers/README.md#locators-for-linux-packages
parsePkgInfo :: (Has Diagnostics sig m, Has Logger sig m) => PkgInfo -> m (Maybe BdbEntry)
parsePkgInfo (PkgInfo (Just pkgName) (Just pkgVersion) (Just pkgRelease) (Just pkgArch) pkgEpoch) = pure $ Just $ BdbEntry pkgArch pkgName (pkgVersion <> "-" <> pkgRelease) (fmap (toText . show) pkgEpoch)
parsePkgInfo pkg = do
  logDebug . pretty $ "Ignoring package '" <> show pkg <> "' is missing one or more fields; all fields are required"
  pure Nothing

-- | Packages are read as a JSON array of base64 strings.
readBerkeleyDB ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  ) =>
  Path Abs File ->
  m [BdbEntry]
readBerkeleyDB file = withBerkeleyBinary $ \bin -> do
  -- Get the process working directory, not the one that 'ReadFS' reports, because 'ReadFS' is inside of the tarball.
  cwd <- getSystemCwd

  -- Read the file content from disk and send it to the parser via stdin.
  -- This is necessary because the parser doesn't have access to the file system being read.
  fileContent <- context "read file content" $ readContentsBS file

  -- Handle the JSON response.
  (bdbJsonOutput :: [Text]) <- context "read raw blobs" . execJson' cwd (bdbCommand bin) . decodeUtf8 $ B64.encode fileContent
  bdbByteOutput <- context "decode base64" (traverse (fromEitherShow . B64.decode . encodeUtf8) bdbJsonOutput)
  entries <- context "parse blobs" (traverse (fromEitherShow . readPackageInfo . BSL.fromStrict) bdbByteOutput)
  context "parse package info" $ do
    list <- catMaybes <$> traverse parsePkgInfo entries
    when (length list /= length entries) $ logWarn "BerkeleyDB has packages that were ignored due to a missing field (arch, package, version, or epoch)."
    pure list

bdbCommand :: BinaryPaths -> Command
bdbCommand bin =
  Command
    { cmdName = toText $ toPath bin
    , cmdArgs = []
    , cmdAllowErr = Never
    }

getSystemCwd :: (Has (Lift IO) sig m, Has Diagnostics sig m) => m (Path Abs Dir)
getSystemCwd = do
  cwd <- fatalOnSomeException "get process working directory" $ sendIO getCurrentDirectory
  fatalOnSomeException "parse cwd as path" . sendIO $ parseAbsDir cwd
