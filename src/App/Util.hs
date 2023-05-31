{-# LANGUAGE DataKinds #-}

module App.Util (
  validateDir,
  validateFile,
  SupportedOS (..),
  runningInOS,
  whenRuningInOS,
  userOverrideCommand,
) where

import App.Types
import Control.Monad (unless, when)
import Data.Map qualified as Map
import Data.Text (Text)
import DepTypes (DepType)
import Path (Abs, File, Path)
import Path.IO qualified as P
import System.Exit (die)
import System.Info qualified as SysInfo

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

-- | The list of supported operating systems for FOSSA CLI.
-- This is intended to be used to support easily changing runtime operations.
--
-- Sometimes different imports are required for different operating systems.
-- In those situations, you'll need to use @{-# LANGUAGE CPP #-}@ and @#ifdef mingw32_HOST_OS@ pragmas instead.
data SupportedOS
  = Windows
  | Linux
  | Darwin

osName :: SupportedOS -> String
osName Windows = "mingw32"
osName Linux = "linux"
osName Darwin = "darwin"

-- | Check if the current program is executing inside the specified operating system.
runningInOS :: SupportedOS -> Bool
runningInOS = (SysInfo.os ==) . osName

-- | Run the applicative function when the current program is executing inside the specified operating system.
whenRuningInOS :: Applicative f => SupportedOS -> f () -> f ()
whenRuningInOS os = when (runningInOS os)

-- | Get the user override command for a given dependency type.
userOverrideCommand :: DepType -> OverrideDynamicAnalysisBinary -> Maybe Text
userOverrideCommand depType = Map.lookup depType . unOverrideDynamicAnalysisBinary
