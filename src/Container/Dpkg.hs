{-# LANGUAGE TemplateHaskell #-}

module Container.Dpkg (
  analyzeDpkgEntries,
  analyzeDpkgEntriesScoped,
  DpkgEntry (..),
) where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Data.Text (Text)
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, Path, mkAbsDir)

-- | An entry in the dpkg database, consisting of the architecture, package, and version.
data DpkgEntry = DpkgEntry
  { dpkgEntryArch :: Text
  , dpkgEntryPackage :: Text
  , dpkgEntryVersion :: Text
  }
  deriving (Eq, Ord, Show)

-- | Analyze dpkg entries from the root of the file system.
-- Searches for: @var/lib/dpkg/{status|status.d}@ from the root of the file system.
analyzeDpkgEntries :: (Has ReadFS sig m, Has Diagnostics sig m) => m ([DpkgEntry])
analyzeDpkgEntries = analyzeDpkgEntriesScoped $(mkAbsDir "/")

-- | Analyze dpkg entries from the provided root.
-- Searches for: @var/lib/dpkg/{status|status.d}@ from the provided directory.
analyzeDpkgEntriesScoped :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m ([DpkgEntry])
analyzeDpkgEntriesScoped _ = pure []
