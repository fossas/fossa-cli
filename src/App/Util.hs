{-# LANGUAGE DataKinds #-}

module App.Util (
  ancestryDerived,
  ancestryDirect,
  validateDir,
  validateFile,
  guardStrictMode,
  FileAncestry (..),
) where

import App.Types
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (Diagnostics)
import Control.Effect.Diagnostics (fatalText)
import Control.Monad (unless)
import Data.String.Conversion (ToText (..))
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, Rel, SomeBase (..), toFilePath, (</>))
import Path.Extra (tryMakeRelative)
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

-- | Guards analysis strategies depending on the mode. On strict mode, emit an error to end the chain of diagnostics to prevent
-- other strategies to be executed. On non-strict mode, allow fallback strategies to be executed.
guardStrictMode :: Has Diagnostics sig m => Mode -> m a -> m a
guardStrictMode Strict _ = fatalText "Strict mode enabled, skipping other strategies"
guardStrictMode NonStrict action = action
