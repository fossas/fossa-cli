{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.EmbeddedBinary (
  extractEmbeddedBinary,
  cleanupExtractedBinaries,
  withEmbeddedBinary,
  dumpEmbeddedBinary,
  toExecutablePath,
  BinaryPaths,
  withWigginsBinary,
  withSyftBinary,
  withThemisBinaryAndIndex,
  allBins,
  PackagedBinary (..),
  dumpSubCommand,
) where

import App.Fossa.Config.DumpBinaries (
  DumpBinsConfig (..),
  DumpBinsOpts,
  mkSubCommand,
 )
import App.Fossa.Subcommand (SubCommand)
import Control.Effect.Exception (bracket)
import Control.Effect.Lift (Has, Lift, sendIO)
import Data.ByteString (ByteString, writeFile)
import Data.FileEmbed.Extra (embedFileIfExists)
import Data.Foldable (for_)
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  fromAbsFile,
  mkRelDir,
  mkRelFile,
  parent,
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
  = Syft
  | Wiggins
  | Themis
  | ThemisIndex
  deriving (Show, Eq, Enum, Bounded)

dumpSubCommand :: SubCommand DumpBinsOpts DumpBinsConfig
dumpSubCommand = mkSubCommand dumpBinsMain

dumpBinsMain :: Has (Lift IO) sig m => DumpBinsConfig -> m ()
dumpBinsMain (DumpBinsConfig dir) = for_ allBins $ dumpEmbeddedBinary dir

allBins :: [PackagedBinary]
allBins = enumFromTo minBound maxBound

data BinaryPaths = BinaryPaths
  { binaryPathContainer :: Path Abs Dir
  , binaryFilePath :: Path Rel File
  }

toExecutablePath :: BinaryPaths -> Path Abs File
toExecutablePath BinaryPaths{..} = binaryPathContainer </> binaryFilePath

withSyftBinary ::
  ( Has (Lift IO) sig m
  ) =>
  (BinaryPaths -> m c) ->
  m c
withSyftBinary = withEmbeddedBinary Syft

withWigginsBinary ::
  ( Has (Lift IO) sig m
  ) =>
  (BinaryPaths -> m c) ->
  m c
withWigginsBinary = withEmbeddedBinary Wiggins

withThemisBinaryAndIndex ::
  ( Has (Lift IO) sig m
  ) =>
  ([BinaryPaths] -> m c) ->
  m c
withThemisBinaryAndIndex = bracket (traverse extractEmbeddedBinary [Themis, ThemisIndex]) (traverse cleanupExtractedBinaries)

withEmbeddedBinary ::
  ( Has (Lift IO) sig m
  ) =>
  PackagedBinary ->
  (BinaryPaths -> m c) ->
  m c
withEmbeddedBinary bin = bracket (extractEmbeddedBinary bin) cleanupExtractedBinaries

cleanupExtractedBinaries :: (Has (Lift IO) sig m) => BinaryPaths -> m ()
cleanupExtractedBinaries (BinaryPaths binPath _) = sendIO $ removeDirRecur binPath

extractEmbeddedBinary :: (Has (Lift IO) sig m) => PackagedBinary -> m BinaryPaths
extractEmbeddedBinary bin = do
  container <- sendIO extractDir
  -- Determine paths to which we should write the binaries
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
writeBinary dest bin = sendIO . writeExecutable dest $ case bin of
  Syft -> embeddedBinarySyft
  Wiggins -> embeddedBinaryWiggins
  Themis -> embeddedBinaryThemis
  ThemisIndex -> embeddedBinaryThemisIndex

writeExecutable :: Path Abs File -> ByteString -> IO ()
writeExecutable path content = do
  ensureDir $ parent path
  writeFile (fromAbsFile path) content
  makeExecutable path

extractedPath :: PackagedBinary -> Path Rel File
extractedPath bin = case bin of
  Syft -> $(mkRelFile "syft")
  -- Rename wiggins upon local extraction so that we can provide a better status line to users during the VSI strategy.
  -- Users don't know what "wiggins" is, but they explicitly enable the VSI plugin, so this is more intuitive.
  Wiggins -> $(mkRelFile "vsi-plugin")
  Themis -> $(mkRelFile "themis-cli")
  ThemisIndex -> $(mkRelFile "index.gob")

extractDir :: Has (Lift IO) sig m => m (Path Abs Dir)
extractDir = do
  wd <- sendIO getTempDir
  pure (wd </> $(mkRelDir "fossa-vendor"))

makeExecutable :: Path Abs File -> IO ()
makeExecutable path = do
  p <- getPermissions path
  setPermissions path (p{executable = True})

-- The intent with these embedded binaries is that the build system will replace the files with
-- built binaries of the appropriate architecture.
-- The below functions are expected to warn since the vendor-bins directory is typically populated in CI.
-- If you wish to run these on your local system, populate these binaries via `vendor_download.sh`.
embeddedBinaryWiggins :: ByteString
embeddedBinaryWiggins = $(embedFileIfExists "vendor-bins/wiggins")

embeddedBinarySyft :: ByteString
embeddedBinarySyft = $(embedFileIfExists "vendor-bins/syft")

embeddedBinaryThemis :: ByteString
embeddedBinaryThemis = $(embedFileIfExists "vendor-bins/themis-cli")

embeddedBinaryThemisIndex :: ByteString
embeddedBinaryThemisIndex = $(embedFileIfExists "vendor-bins/index.gob")