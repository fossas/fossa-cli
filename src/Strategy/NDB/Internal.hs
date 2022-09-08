module Strategy.NDB.Internal (
  readNDB,
  NdbEntry (..),
) where

import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withBerkeleyBinary)
import Control.Effect.Diagnostics (Diagnostics, context, fatalOnSomeException, fatalText, fromEitherShow)
import Control.Effect.Lift (Lift, sendIO)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BSL
import Data.Rpm.DbHeaderBlob (PkgInfo (..), readPackageInfo)
import Data.String.Conversion (ConvertUtf8 (encodeUtf8), decodeUtf8, toText)
import Data.Text (Text)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execJson')
import Effect.ReadFS (Has, ReadFS, readContentsBS)
import Path (Abs, Dir, File, Path, parseAbsDir)
import System.Directory (getCurrentDirectory)

-- | An entry in the database, consisting of the architecture, package, and version.
data NdbEntry = NdbEntry
  { ndbEntryArch :: Text
  , ndbEntryPackage :: Text
  , ndbEntryVersion :: Text
  , ndbEntryEpoch :: Maybe Text
  }
  deriving (Eq, Ord, Show)

-- | FOSSA _requires_ that architecture is provided: https://github.com/fossas/FOSSA/blob/e61713dec1ef80dc6b6114f79622c14df5278235/modules/fetchers/README.md#locators-for-linux-packages
parsePkgInfo :: (Has Diagnostics sig m) => PkgInfo -> m NdbEntry
parsePkgInfo (PkgInfo (Just pkgName) (Just pkgVersion) (Just pkgRelease) (Just pkgArch) pkgEpoch) = pure $ NdbEntry pkgArch pkgName (pkgVersion <> "-" <> pkgRelease) (fmap (toText . show) pkgEpoch)
parsePkgInfo pkg = fatalText . toText $ "package '" <> show pkg <> "' is missing one or more fields; all fields are required"

-- | Packages are read as a JSON array of base64 strings.
readNDB ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  Path Abs File ->
  m [NdbEntry]
readNDB file = withBerkeleyBinary $ \bin -> do
  -- Get the process working directory, not the one that 'ReadFS' reports, because 'ReadFS' is inside of the tarball.
  cwd <- getSystemCwd

  -- Read the file content from disk and send it to the parser via stdin.
  -- This is necessary because the parser doesn't have access to the file system being read.
  fileContent <- context "read file content" $ readContentsBS file

  -- Handle the JSON response.
  (json :: [Text]) <- context "read raw blobs" . execJson' cwd (command bin) . decodeUtf8 $ B64.encode fileContent
  decoded <- context "decode base64" . traverse fromEitherShow $ B64.decode <$> fmap encodeUtf8 json
  entries <- context "parse blobs" . traverse fromEitherShow $ readPackageInfo <$> fmap BSL.fromStrict decoded
  context "parse package info" $ traverse parsePkgInfo entries

command :: BinaryPaths -> Command
command bin =
  Command
    { cmdName = toText $ toPath bin
    , cmdArgs = []
    , cmdAllowErr = Never
    }

getSystemCwd :: (Has (Lift IO) sig m, Has Diagnostics sig m) => m (Path Abs Dir)
getSystemCwd = do
  cwd <- fatalOnSomeException "get process working directory" $ sendIO getCurrentDirectory
  fatalOnSomeException "parse cwd as path" . sendIO $ parseAbsDir cwd
