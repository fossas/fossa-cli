module Strategy.Python.Poetry.PoetryLock (
  PoetryLock (..),
  PoetryLockPackage (..),
  PackageName (..),
  PoetryLockPackageSource (..),
  PoetryLockDependencySpec (..),
  poetryLockCodec,

  -- * for testing only
  ObjectVersion (..),
  PoetryMetadata (..),
) where

import Control.Applicative (Alternative ((<|>)))
import Data.Map (Map)
import Data.Text (Text)
import Toml (TomlCodec, (.=))
import Toml qualified

-- | Represents the content of the poetry lock file.
data PoetryLock = PoetryLock
  { poetryLockPackages :: [PoetryLockPackage]
  , poetryLockMetadata :: PoetryMetadata
  }
  deriving (Eq, Ord, Show)

newtype PackageName = PackageName {unPackageName :: Text} deriving (Eq, Ord, Show)

poetryLockCodec :: TomlCodec PoetryLock
poetryLockCodec =
  PoetryLock
    <$> Toml.list poetryLockPackageCodec "package"
    .= poetryLockPackages
    <*> Toml.table poetryMetadataCodec "metadata"
    .= poetryLockMetadata

-- | Metadata of poetry lock file.
data PoetryMetadata = PoetryMetadata
  { poetryMetadataLockVersion :: Text
  , poetryMetadataContentHash :: Text
  , poetryMetadataPythonVersions :: Text
  }
  deriving (Eq, Ord, Show)

poetryMetadataCodec :: TomlCodec PoetryMetadata
poetryMetadataCodec =
  PoetryMetadata
    <$> Toml.text "lock-version"
    .= poetryMetadataLockVersion
    <*> Toml.text "content-hash"
    .= poetryMetadataContentHash
    <*> Toml.text "python-versions"
    .= poetryMetadataPythonVersions

-- | A PoetryLockPackageSource represents [package.source] field in poetry.lock.
-- Source indicates from where the package was retrieved.
data PoetryLockPackageSource = PoetryLockPackageSource
  { poetryLockPackageSourceType :: Text
  , poetryLockPackageSourceUrl :: Text
  , poetryLockPackageSourceReference :: Maybe Text
  , poetryLockPackageSourceResolvedReference :: Maybe Text
  }
  deriving (Eq, Ord, Show)

-- | Poetry package entry found in poetry lock file.
data PoetryLockPackage = PoetryLockPackage
  { poetryLockPackageName :: PackageName
  , poetryLockPackageVersion :: Text
  , poetryLockPackageCategory :: Maybe Text
  , poetryLockPackageOptional :: Bool
  , poetryLockPackagePythonVersions :: Text
  , poetryLockPackageDependencies :: Map Text PoetryLockDependencySpec
  , poetryLockPackageSource :: Maybe PoetryLockPackageSource
  }
  deriving (Eq, Ord, Show)

poetryLockPackageCodec :: TomlCodec PoetryLockPackage
poetryLockPackageCodec =
  PoetryLockPackage
    <$> Toml.diwrap (Toml.text "name")
    .= poetryLockPackageName
    <*> Toml.text "version"
    .= poetryLockPackageVersion
    <*> Toml.dioptional (Toml.text "category")
    .= poetryLockPackageCategory
    <*> Toml.bool "optional"
    .= poetryLockPackageOptional
    <*> Toml.text "python-versions"
    .= poetryLockPackagePythonVersions
    <*> Toml.tableMap Toml._KeyText poetryLockPackagePoetryLockDependencySpecCodec "dependencies"
    .= poetryLockPackageDependencies
    <*> Toml.dioptional (Toml.table poetryLockPackageSourceCodec "source")
    .= poetryLockPackageSource

poetryLockPackageSourceCodec :: TomlCodec PoetryLockPackageSource
poetryLockPackageSourceCodec =
  PoetryLockPackageSource
    <$> Toml.text "type"
    .= poetryLockPackageSourceType
    <*> Toml.text "url"
    .= poetryLockPackageSourceUrl
    <*> Toml.dioptional (Toml.text "reference")
    .= poetryLockPackageSourceReference
    <*> Toml.dioptional (Toml.text "resolved_reference")
    .= poetryLockPackageSourceResolvedReference

data PoetryLockDependencySpec
  = TextVersion Text
  | ObjectVersionSpec ObjectVersion
  | MultipleObjectVersionSpec [ObjectVersion]
  deriving (Eq, Ord, Show)

newtype ObjectVersion = ObjectVersion
  { unObjectVersion :: Text
  }
  deriving (Eq, Ord, Show)

objectVersionCodec :: TomlCodec ObjectVersion
objectVersionCodec =
  ObjectVersion
    <$> Toml.text "version"
    .= unObjectVersion

matchTextVersion :: PoetryLockDependencySpec -> Maybe Text
matchTextVersion (TextVersion version) = Just version
matchTextVersion _ = Nothing

matchObjectVersionSpec :: PoetryLockDependencySpec -> Maybe ObjectVersion
matchObjectVersionSpec (ObjectVersionSpec version) = Just version
matchObjectVersionSpec _ = Nothing

matchMultipleObjectVersionSpec :: PoetryLockDependencySpec -> Maybe [ObjectVersion]
matchMultipleObjectVersionSpec (MultipleObjectVersionSpec version) = Just version
matchMultipleObjectVersionSpec _ = Nothing

poetryLockPackagePoetryLockDependencySpecCodec :: Toml.Key -> TomlCodec PoetryLockDependencySpec
poetryLockPackagePoetryLockDependencySpecCodec key =
  Toml.dimatch matchTextVersion TextVersion (Toml.text key)
    <|> Toml.dimatch matchObjectVersionSpec ObjectVersionSpec (Toml.table objectVersionCodec key)
    <|> Toml.dimatch matchMultipleObjectVersionSpec MultipleObjectVersionSpec (Toml.list objectVersionCodec key)
