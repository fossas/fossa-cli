{-# LANGUAGE TemplateHaskell #-}

module App.VPSScan.EmbeddedBinary 
  ( BinaryPaths(..)
  , extractEmbeddedBinaries
  , cleanupExtractedBinaries
  ) where

import Prelude hiding (writeFile)
import Control.Monad.IO.Class
import Data.ByteString (writeFile, ByteString)
import Path
import Path.IO
import Data.FileEmbed.Extra

data BinaryPaths = BinaryPaths
  { binaryPathContainer :: Path Abs Dir
  , ramjetBinaryPath :: Path Abs File
  , nomosBinaryPath :: Path Abs File
  , pathfinderBinaryPath :: Path Abs File
  , sherlockBinaryPath :: Path Abs File
  }

cleanupExtractedBinaries :: (MonadIO m) => BinaryPaths -> m ()
cleanupExtractedBinaries BinaryPaths{..} = do
  removeDirRecur binaryPathContainer
  pure ()

extractEmbeddedBinaries :: (MonadIO m) => m BinaryPaths
extractEmbeddedBinaries = do
  container <- extractDir

  -- Determine paths to which we should write the binaries
  ramjetBinaryPath <- extractedPath $(mkRelFile "ramjet-cli-ipr")
  nomosBinaryPath <- extractedPath $(mkRelFile "nomossa")
  pathfinderBinaryPath <- extractedPath $(mkRelFile "pathfinder")
  sherlockBinaryPath <- extractedPath $(mkRelFile "sherlock-cli")

  -- Write the binaries
  liftIO $ writeExecutable ramjetBinaryPath embeddedBinaryRamjetCli
  liftIO $ writeExecutable nomosBinaryPath embeddedBinaryNomossa
  liftIO $ writeExecutable pathfinderBinaryPath embeddedBinaryPathfinder
  liftIO $ writeExecutable sherlockBinaryPath embeddedBinarySherlockCli

  -- Return the paths
  pure (BinaryPaths container ramjetBinaryPath nomosBinaryPath pathfinderBinaryPath sherlockBinaryPath)

writeExecutable :: Path Abs File -> ByteString -> IO ()
writeExecutable path content = do
  ensureDir $ parent path
  writeFile (fromAbsFile path) content
  makeExecutable path

extractedPath :: MonadIO m => Path Rel File -> m (Path Abs File)
extractedPath name = do
  dir <- extractDir
  pure (dir </> name)

extractDir :: MonadIO m => m (Path Abs Dir)
extractDir = do
  wd <- liftIO getTempDir
  pure (wd </> $(mkRelDir "vpscli-vendor"))

makeExecutable :: Path Abs File -> IO ()
makeExecutable path = do
  p <- getPermissions path
  setPermissions path (p {executable = True})
  
-- The intent with these embedded binaries is that the build system will replace the files with built binaries of the appropriate architecture.
-- The versions vendored into the repository are suitable for running on MacOS.
-- The below functions are expectd to warn since the vendor directory is typically populated in CI.
-- If you wish to build `vpscli` for your local system, populate these binaries via `vendor_download.sh`.
embeddedBinarySherlockCli :: ByteString
embeddedBinarySherlockCli = $(embedFileIfExists "vendor/sherlock-cli")

embeddedBinaryRamjetCli :: ByteString
embeddedBinaryRamjetCli = $(embedFileIfExists "vendor/ramjet-cli-ipr")

embeddedBinaryPathfinder :: ByteString
embeddedBinaryPathfinder = $(embedFileIfExists "vendor/pathfinder")

embeddedBinaryNomossa :: ByteString
embeddedBinaryNomossa = $(embedFileIfExists "vendor/nomossa")
