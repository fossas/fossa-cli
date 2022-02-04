module App.Fossa.VSI.DynLinked.Types (
  LinuxPackageManager (..),
  LinuxPackageMetadata (..),
  LinuxDistro (..),
  DynamicDependency (..),
  ResolvedLinuxPackage (..),
) where

import Data.Text (Text)
import Path (Abs, File, Path)

data LinuxPackageManager
  = LinuxPackageManagerRPM
  | LinuxPackageManagerDEB
  | LinuxPackageManagerAPK
  deriving (Eq, Ord, Show)

-- | These are all opaque blobs, we're not doing any processing on them other than rendering into a @Locator@.
-- For that reason, stick to @Text@.
data LinuxPackageMetadata = LinuxPackageMetadata
  { linuxPackageID :: Text
  , linuxPackageRevision :: Text
  , linuxPackageArch :: Text
  , linuxPackageDistroEpoch :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data LinuxDistro = LinuxDistro
  { linuxDistroName :: Text
  , linuxDistroRelease :: Text
  }
  deriving (Eq, Ord, Show)

data ResolvedLinuxPackage = ResolvedLinuxPackage
  { resolvedLinuxPackageManager :: LinuxPackageManager
  , resolvedLinuxPackageMetadata :: LinuxPackageMetadata
  }
  deriving (Eq, Ord, Show)

data DynamicDependency = DynamicDependency
  { dynamicDependencyDiskPath :: Path Abs File
  , dynamicDependencyResolved :: Maybe ResolvedLinuxPackage
  }
  deriving (Eq, Ord, Show)
