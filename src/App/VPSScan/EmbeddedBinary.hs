{-# LANGUAGE TemplateHaskell #-}

module App.VPSScan.EmbeddedBinary 
  ( withUnpackedSherlockCli
  , withUnpackedIPRClis
  , IPRBinaryPaths(..)
  ) where

import Prelude hiding (writeFile)
import Control.Effect.Exception
import Control.Monad.IO.Class
import Data.ByteString (writeFile, ByteString)
import Path
import Path.IO
import Data.FileEmbed.Extra

withUnpackedSherlockCli :: (Has (Lift IO) sig m, MonadIO m) => (Path Abs File -> m a) -> m a
withUnpackedSherlockCli act =
  bracket (liftIO getTempDir >>= \tmp -> createTempDir tmp "fossa-vpscli-vendor-sherlock")
          (liftIO . removeDirRecur)
          go
  where
    go tmpDir = do
      let binaryPath = tmpDir </> $(mkRelFile "sherlock-cli")
      liftIO (writeFile (fromAbsFile binaryPath) embeddedBinarySherlockCli)
      liftIO (makeExecutable binaryPath)
      act binaryPath

data IPRBinaryPaths = IPRBinaryPaths
  { ramjetBinaryPath :: Path Abs File
  , nomosBinaryPath :: Path Abs File
  , pathfinderBinaryPath :: Path Abs File
  }

withUnpackedIPRClis :: (Has (Lift IO) sig m, MonadIO m) => (IPRBinaryPaths -> m a) -> m a
withUnpackedIPRClis act = 
  bracket (liftIO getTempDir >>= \tmp -> createTempDir tmp "fossa-vpscli-vendor-ipr")
          (liftIO . removeDirRecur)
          go
  where
    go tmpDir = do
      let paths = IPRBinaryPaths (tmpDir </> $(mkRelFile "ramjet-cli-ipr")) (tmpDir </> $(mkRelFile "nomossa")) (tmpDir </> $(mkRelFile "pathfinder"))
      liftIO (writeFile (fromAbsFile $ ramjetBinaryPath paths) embeddedBinaryRamjetCli)
      liftIO (writeFile (fromAbsFile $ nomosBinaryPath paths) embeddedBinaryNomossa)
      liftIO (writeFile (fromAbsFile $ pathfinderBinaryPath paths) embeddedBinaryPathfinder)
      liftIO (makeExecutable (ramjetBinaryPath paths))
      liftIO (makeExecutable (nomosBinaryPath paths))
      liftIO (makeExecutable (pathfinderBinaryPath paths))
      act paths

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
