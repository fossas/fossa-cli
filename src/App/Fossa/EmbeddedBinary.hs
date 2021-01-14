{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.EmbeddedBinary
  ( extractEmbeddedBinary,
    cleanupExtractedBinaries,
    withEmbeddedBinary,
    dumpEmbeddedBinary,
    toExecutablePath,
    BinaryPaths,
    withWigginsBinary,
    withSyftBinary,
    allBins,
    PackagedBinary (..)
  )
where

import Control.Effect.Exception (bracket)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad.IO.Class
import Data.ByteString (ByteString, writeFile)
import Data.FileEmbed.Extra
import Path
import Path.IO
import Prelude hiding (writeFile)

data PackagedBinary
  = Syft
  | Wiggins
  deriving (Show, Eq, Enum, Bounded)

allBins :: [PackagedBinary]
allBins = enumFromTo minBound maxBound

data BinaryPaths
  = BinaryPaths
      { binaryPathContainer :: Path Abs Dir,
        binaryFilePath :: Path Rel File
      }

toExecutablePath :: BinaryPaths -> Path Abs File
toExecutablePath BinaryPaths {..} = binaryPathContainer </> binaryFilePath

withSyftBinary ::
  ( Has (Lift IO) sig m,
    MonadIO m
  ) =>
  (BinaryPaths -> m c) ->
  m c
withSyftBinary = withEmbeddedBinary Syft

withWigginsBinary ::
  ( Has (Lift IO) sig m,
    MonadIO m
  ) =>
  (BinaryPaths -> m c) ->
  m c
withWigginsBinary = withEmbeddedBinary Wiggins

withEmbeddedBinary ::
  ( Has (Lift IO) sig m,
    MonadIO m
  ) =>
  PackagedBinary ->
  (BinaryPaths -> m c) ->
  m c
withEmbeddedBinary bin = bracket (extractEmbeddedBinary bin) cleanupExtractedBinaries

cleanupExtractedBinaries :: (MonadIO m) => BinaryPaths -> m ()
cleanupExtractedBinaries (BinaryPaths binPath _) = removeDirRecur binPath

extractEmbeddedBinary :: (MonadIO m) => PackagedBinary -> m BinaryPaths
extractEmbeddedBinary bin = do
  container <- extractDir
  -- Determine paths to which we should write the binaries
  let binPath = extractedPath bin
  -- Write the binary
  liftIO $ writeBinary (container </> binPath) bin
  -- Return the paths
  pure (BinaryPaths container binPath)

dumpEmbeddedBinary :: Has (Lift IO) sig m => Path Abs Dir -> PackagedBinary -> m ()
dumpEmbeddedBinary dir bin = writeBinary path bin
  where path = dir </> extractedPath bin

writeBinary :: (Has (Lift IO) sig m) => Path Abs File -> PackagedBinary -> m ()
writeBinary dest bin = sendIO . writeExecutable dest $ case bin of
  Syft -> embeddedBinarySyft
  Wiggins -> embeddedBinaryWiggins

writeExecutable :: Path Abs File -> ByteString -> IO ()
writeExecutable path content = do
  ensureDir $ parent path
  writeFile (fromAbsFile path) content
  makeExecutable path

extractedPath :: PackagedBinary -> Path Rel File
extractedPath bin = case bin of
  Syft -> $(mkRelFile "syft")
  Wiggins -> $(mkRelFile "wiggins")

extractDir :: MonadIO m => m (Path Abs Dir)
extractDir = do
  wd <- liftIO getTempDir
  pure (wd </> $(mkRelDir "fossa-vendor"))

makeExecutable :: Path Abs File -> IO ()
makeExecutable path = do
  p <- getPermissions path
  setPermissions path (p {executable = True})

-- The intent with these embedded binaries is that the build system will replace the files with
-- built binaries of the appropriate architecture.
-- The below functions are expected to warn since the vendor directory is typically populated in CI.
-- If you wish to run these on your local system, populate these binaries via `vendor_download.sh`.
embeddedBinaryWiggins :: ByteString
embeddedBinaryWiggins = $(embedFileIfExists "vendor/wiggins")

embeddedBinarySyft :: ByteString
embeddedBinarySyft = $(embedFileIfExists "vendor/syft")
