{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.EmbeddedBinary (
  BinaryPaths,
  Ficus,
  Lernie,
  ThemisIndex,
  ThemisBins (..),
  toPath,
  withThemisAndIndex,
  withBerkeleyBinary,
  withFicusBinary,
  withLernieBinary,
  withMillhoneBinary,
  withCirceBinary,
  allBins,
  dumpEmbeddedBinary,
  themisVersion,
) where

import App.Version.TH (themisVersionQ)
import Codec.Compression.Lzma qualified as Lzma
import Control.Effect.Exception (bracket)
import Control.Effect.Lift (Has, Lift, sendIO)
import Data.ByteString (ByteString, writeFile)
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed.Extra (embedFileIfExists)
import Data.Foldable (traverse_)
import Data.String.Conversion (toLazy, toString)
import Data.Tagged (Tagged, applyTag, unTag)
import Data.Text (Text)
import Data.UUID qualified as UUID (toString)
import Data.UUID.V4 qualified as UUID (nextRandom)
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  fromAbsFile,
  mkRelFile,
  parent,
  parseRelDir,
  (</>),
 )
import Path.IO (
  Permissions (executable),
  ensureDir,
  getPermissions,
  getTempDir,
  removeDirRecur,
  setPermissions,
 )
import Prelude hiding (writeFile)

data PackagedBinary
  = Themis
  | ThemisIndex
  | BerkeleyDB
  | Ficus
  | Lernie
  | Millhone
  | Circe
  deriving (Show, Eq, Enum, Bounded)

allBins :: [PackagedBinary]
allBins = enumFromTo minBound maxBound

data BinaryPaths = BinaryPaths
  { binaryPathContainer :: Path Abs Dir
  , binaryFilePath :: Path Rel File
  }
  deriving (Eq, Ord, Show)

data ThemisBinary

data ThemisIndex

data Lernie

data Ficus

data Circe

data ThemisBins = ThemisBins
  { themisBinaryPaths :: Tagged ThemisBinary BinaryPaths
  , indexBinaryPaths :: Tagged ThemisIndex BinaryPaths
  }

toPath :: BinaryPaths -> Path Abs File
toPath BinaryPaths{..} = binaryPathContainer </> binaryFilePath

cleanupThemisBins :: Has (Lift IO) sig m => ThemisBins -> m ()
cleanupThemisBins (ThemisBins a b) = traverse_ cleanupExtractedBinaries [unTag a, unTag b]

withThemisAndIndex :: Has (Lift IO) sig m => (ThemisBins -> m c) -> m c
withThemisAndIndex = bracket extractThemisFiles cleanupThemisBins

-- When running Themis we always need both the themis-cli and the decompressed index.gob
extractThemisFiles :: Has (Lift IO) sig m => m ThemisBins
extractThemisFiles = do
  themisBin <- extractEmbeddedBinary Themis
  let themisActual = applyTag @ThemisBinary themisBin
  -- TODO: refactor into `extractXZippedBinary ThemisIndex`
  -- decompress index.gob.xz binary and write it to disk
  container <- sendIO extractDir
  let decompressedThemisIndex =
        BinaryPaths
          { binaryPathContainer = container
          , binaryFilePath = $(mkRelFile "index.gob")
          }
  _ <- sendIO $ ensureDir $ parent $ toPath decompressedThemisIndex
  _ <- sendIO $ BL.writeFile (toString $ toPath decompressedThemisIndex) (Lzma.decompress $ toLazy embeddedBinaryThemisIndex)
  pure $ ThemisBins themisActual $ applyTag @ThemisIndex decompressedThemisIndex

withBerkeleyBinary :: (Has (Lift IO) sig m) => (BinaryPaths -> m c) -> m c
withBerkeleyBinary = withEmbeddedBinary BerkeleyDB

withLernieBinary :: (Has (Lift IO) sig m) => (BinaryPaths -> m c) -> m c
withLernieBinary = withEmbeddedBinary Lernie

withFicusBinary :: (Has (Lift IO) sig m) => (BinaryPaths -> m c) -> m c
withFicusBinary = withEmbeddedBinary Ficus

withMillhoneBinary :: (Has (Lift IO) sig m) => (BinaryPaths -> m c) -> m c
withMillhoneBinary = withEmbeddedBinary Millhone

withCirceBinary :: (Has (Lift IO) sig m) => (BinaryPaths -> m c) -> m c
withCirceBinary = withEmbeddedBinary Circe

withEmbeddedBinary :: (Has (Lift IO) sig m) => PackagedBinary -> (BinaryPaths -> m c) -> m c
withEmbeddedBinary bin = bracket (extractEmbeddedBinary bin) cleanupExtractedBinaries

cleanupExtractedBinaries :: (Has (Lift IO) sig m) => BinaryPaths -> m ()
cleanupExtractedBinaries (BinaryPaths binPath _) = sendIO $ removeDirRecur binPath

extractEmbeddedBinary :: (Has (Lift IO) sig m) => PackagedBinary -> m BinaryPaths
extractEmbeddedBinary bin = do
  container <- sendIO extractDir
  let binPath = extractedPath bin
  -- Write the binary
  sendIO $ writeBinary (container </> binPath) bin
  -- Return the paths
  pure (BinaryPaths container binPath)

dumpEmbeddedBinary :: Has (Lift IO) sig m => Path Abs Dir -> PackagedBinary -> m ()
dumpEmbeddedBinary dir bin = writeBinary path bin
  where
    path = dir </> extractedPath bin

writeBinary :: (Has (Lift IO) sig m) => Path Abs File -> PackagedBinary -> m ()
writeBinary dest bin = do
  sendIO . writeExecutable dest $ case bin of
    Themis -> embeddedBinaryThemis
    ThemisIndex -> embeddedBinaryThemisIndex
    BerkeleyDB -> embeddedBinaryBerkeleyDB
    Ficus -> embeddedBinaryFicus
    Lernie -> embeddedBinaryLernie
    Millhone -> embeddedBinaryMillhone
    Circe -> embeddedBinaryCirce

writeExecutable :: Path Abs File -> ByteString -> IO ()
writeExecutable path content = do
  ensureDir $ parent path
  writeFile (fromAbsFile path) content
  makeExecutable path

extractedPath :: PackagedBinary -> Path Rel File
extractedPath bin = case bin of
  Themis -> $(mkRelFile "themis-cli")
  ThemisIndex -> $(mkRelFile "index.gob.xz")
  BerkeleyDB -> $(mkRelFile "berkeleydb-plugin")
  Ficus -> $(mkRelFile "ficus")
  Lernie -> $(mkRelFile "lernie")
  Millhone -> $(mkRelFile "millhone")
  Circe -> $(mkRelFile "circe")

-- | Extract to @$TMP/fossa-vendor-<uuid>/
--
-- This function used to extract everything to @$TMP/fossa-vendor@, but there's a subtle issue with that:
-- cleanup just removes the directory where the file resides, which is fine unless there's
-- more than one active extracted file; in which case cleanup would clobber files in use by another function.
--
-- Extracting to another subdir named with the UUID would mean that the cleanup only cleans the UUID subdir,
-- and also doesn't fully isolate functions.
--
-- Instead, this function just appends the UUID to the directory so that subsequent calls are fully isolated.
extractDir :: Has (Lift IO) sig m => m (Path Abs Dir)
extractDir = do
  wd <- sendIO getTempDir
  ts <- sendIO $ UUID.toString <$> UUID.nextRandom
  subDir <- sendIO . parseRelDir $ "fossa-vendor-" <> ts
  pure $ wd </> subDir

makeExecutable :: Path Abs File -> IO ()
makeExecutable path = do
  p <- getPermissions path
  setPermissions path (p{executable = True})

-- The intent with these embedded binaries is that the build system will replace the files with
-- built binaries of the appropriate architecture.
-- The below functions are expected to warn since the vendor-bins directory is typically populated in CI.
-- If you wish to run these on your local system, populate these binaries via `vendor_download.sh`.
--
-- WARNING: If you add new binaries here, please also add them to extra-source-files in spectrometer.cabal.
-- This makes sure that cabal tracks changes to them and will trigger a rebuild.
embeddedBinaryThemis :: ByteString
embeddedBinaryThemis = $(embedFileIfExists "vendor-bins/themis-cli")

embeddedBinaryThemisIndex :: ByteString
embeddedBinaryThemisIndex = $(embedFileIfExists "vendor-bins/index.gob.xz")

themisVersion :: Text
themisVersion = $$(themisVersionQ)

embeddedBinaryLernie :: ByteString
embeddedBinaryLernie = $(embedFileIfExists "vendor-bins/lernie")

embeddedBinaryCirce :: ByteString
embeddedBinaryCirce = $(embedFileIfExists "vendor-bins/circe")

embeddedBinaryFicus :: ByteString
embeddedBinaryFicus = $(embedFileIfExists "vendor-bins/ficus")

-- To build this, run `make build` or `cargo build --release`.
#ifdef mingw32_HOST_OS
embeddedBinaryMillhone :: ByteString
embeddedBinaryMillhone = $(embedFileIfExists "target/release/millhone.exe")
#else
embeddedBinaryMillhone :: ByteString
embeddedBinaryMillhone = $(embedFileIfExists "target/release/millhone")
#endif

-- To build this, run `make build` or `cargo build --release`.
#ifdef mingw32_HOST_OS
embeddedBinaryBerkeleyDB :: ByteString
embeddedBinaryBerkeleyDB = $(embedFileIfExists "target/release/berkeleydb.exe")
#else
embeddedBinaryBerkeleyDB :: ByteString
embeddedBinaryBerkeleyDB = $(embedFileIfExists "target/release/berkeleydb")
#endif
