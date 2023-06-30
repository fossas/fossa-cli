{-# LANGUAGE DataKinds #-}

module App.Util (
  ancestryDerived,
  ancestryDirect,
  validateDir,
  validateFile,
  SupportedOS (..),
  runningInOS,
  whenRuningInOS,
  userOverrideCommand,
  FileAncestry (..),
) where

import App.Types
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (Diagnostics, fatalText)
import Control.Monad (unless, when)
import Data.Map qualified as Map
import Data.String.Conversion (ToText (..))
import Data.Text (Text)
import DepTypes (DepType)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, Rel, SomeBase (..), toFilePath, (</>))
import Path.Extra (tryMakeRelative)
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
-- In those situations, you'll need to use @{\-# LANGUAGE CPP #-\}@ and @#ifdef mingw32_HOST_OS@ pragmas instead.
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
-- | Renders the relative path from the provided directory to the file.
-- If the path cannot be made relative, fatally exits through the diagnostic effect.
ancestryDirect :: Has Diagnostics sig m => Path Abs Dir -> Path Abs File -> m (Path Rel File)
ancestryDirect dir file = case tryMakeRelative dir file of
  Abs _ -> fatalText $ "failed to make " <> toText (toFilePath file) <> " relative to " <> toText (toFilePath dir)
  Rel rel -> pure rel

newtype FileAncestry = FileAncestry {fileAncestryPath :: Path Rel Dir} deriving (Eq, Ord, Show, Generic)

-- | Renders the relative path from the provided directory to the file, prepended with the provided relative directory as a parent.
-- If the path cannot be made relative, fatally exits through the diagnostic effect.
ancestryDerived :: Has Diagnostics sig m => FileAncestry -> Path Abs Dir -> Path Abs File -> m (Path Rel File)
ancestryDerived parent dir file = do
  rel <- ancestryDirect dir file
  pure $ fileAncestryPath parent </> rel
