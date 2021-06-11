{-# LANGUAGE DataKinds #-}

module App.Util (
  validateDir,
  validateFile,
) where

import App.Types
import Control.Monad (unless)
import Path (Abs, File, Path)
import Path.IO qualified as P
import System.Exit (die)

-- | Validate that a filepath points to a directory and the directory exists
validateDir :: FilePath -> IO BaseDir
validateDir dir = do
  absolute <- P.resolveDir' dir
  exists <- P.doesDirExist absolute

  unless exists (die $ "ERROR: Directory " <> show absolute <> " does not exist")
  pure $ BaseDir absolute

-- | Validate that a filepath points to a file and the file exists
validateFile :: FilePath -> IO (Path Abs File)
validateFile file = do
  absolute <- P.resolveFile' file
  exists <- P.doesFileExist absolute
  unless exists (die $ "ERROR: File " <> show absolute <> " does not exist")
  pure absolute
