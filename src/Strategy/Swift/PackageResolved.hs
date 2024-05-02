module Strategy.Swift.PackageResolved (
  SwiftPackageResolvedFile (..),
  SwiftResolvedPackage (..),
  resolvedDependenciesOf,
  parsePackageResolved,
) where

import Control.Carrier.Diagnostics (Diagnostics, Has, context, warn)
import Control.Monad (when)
import Data.Aeson (
  FromJSON (parseJSON),
  Key,
  Object,
  withObject,
  (.:),
  (.:?),
 )
import Data.Aeson.Types (Parser)
import Data.Foldable (asum)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import DepTypes (DepType (GitType), Dependency (..), VerConstraint (CEq))
import Effect.ReadFS (ReadFS, readContentsJson)
import Path (Abs, File, Path)

parsePackageResolved :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m SwiftPackageResolvedFile
parsePackageResolved path = context "read and parse Package.resolved" $ do
  packageResolved <- readContentsJson path

  let fileVersion = version packageResolved
  let parsedVersion = parserVersion $ parserForVersion fileVersion
  when (parsedVersion /= fileVersion) $ warn $ "Package.resolved file is version '" <> show fileVersion <> "', but was parsed as version '" <> show parsedVersion <> "'"

  pure packageResolved

data SwiftPackageResolvedFile = SwiftPackageResolvedFile
  { version :: Integer
  , pinnedPackages :: [SwiftResolvedPackage]
  }
  deriving (Show, Eq, Ord)

data SwiftResolvedPackage = SwiftResolvedPackage
  { package :: Text
  , repositoryURL :: Text
  , repositoryBranch :: Maybe Text
  , repositoryRevision :: Maybe Text
  , repositoryVersion :: Maybe Text
  }
  deriving (Show, Eq, Ord)

-- | If you update this, make sure to update warnAssumedVersion with the new assumed version.
instance FromJSON SwiftPackageResolvedFile where
  parseJSON = withObject "Package.resolved content" $ \obj -> do
    version :: Integer <- obj .: "version"
    (parserFunction $ parserForVersion version) version obj

(|>) :: FromJSON a => Parser Object -> Key -> Parser a
(|>) parser key = do
  obj <- parser
  obj .: key

(|?>) :: FromJSON a => Parser (Maybe Object) -> Key -> Parser (Maybe a)
(|?>) parser key = do
  obj <- parser
  case obj of
    Nothing -> pure Nothing
    Just o -> o .:? key

data PackageResolvedParser = PackageResolvedParser
  { parserVersion :: Integer
  , parserFunction :: (Integer -> Object -> Parser SwiftPackageResolvedFile)
  }

-- | Select the appropriate parser based on the file version.
-- Defaults to the parser with the latest version if none is specified.
parserForVersion :: Integer -> PackageResolvedParser
parserForVersion version = fromMaybe (NE.head allParsers) findVersion
  where
    findVersion :: Maybe PackageResolvedParser
    findVersion = find (\p -> version == parserVersion p) $ NE.toList allParsers

    -- Rather than paying the cost of sorting at runtime, the parent function assumes the "default" parser is the first in the list.
    -- Ensure when adding new parsers that the new parser is added in the appropriate place based on this.
    allParsers :: NonEmpty PackageResolvedParser
    allParsers = PackageResolvedParser 3 parseV3 :| [PackageResolvedParser 2 parseV2, PackageResolvedParser 1 parseV1]

-- | From the Swift Package Manager source code, the pins did not change in v3:
-- https://github.com/apple/swift-package-manager/blob/9aa348e8eecc44fb6f93e1ef46e6dbd29947f4e7/Sources/PackageGraph/PinsStore.swift#L470
parseV3 :: Integer -> Object -> Parser SwiftPackageResolvedFile
parseV3 = parseV2

parseV2 :: Integer -> Object -> Parser SwiftPackageResolvedFile
parseV2 version obj = SwiftPackageResolvedFile version <$> ((obj .: "pins") >>= traverse (withObject "Package.resolved v2 pin" parseV2Pin))

parseV2Pin :: Object -> Parser SwiftResolvedPackage
parseV2Pin obj =
  SwiftResolvedPackage
    <$> obj .: "identity"
    <*> obj .: "location"
    <*> pure Nothing
    <*> (obj .:? "state" |?> "revision")
    <*> (obj .:? "state" |?> "version")

parseV1 :: Integer -> Object -> Parser SwiftPackageResolvedFile
parseV1 version obj = SwiftPackageResolvedFile version <$> ((obj .: "object" |> "pins") >>= traverse (withObject "Package.resolved v1 pin" parseV1Pin))

parseV1Pin :: Object -> Parser SwiftResolvedPackage
parseV1Pin obj =
  SwiftResolvedPackage
    <$> obj .: "package"
    <*> obj .: "repositoryURL"
    <*> (obj .:? "state" |?> "branch")
    <*> (obj .:? "state" |?> "revision")
    <*> (obj .:? "state" |?> "version")

instance FromJSON SwiftResolvedPackage where
  parseJSON = withObject "Package.resolved pinned object" $ \obj ->
    SwiftResolvedPackage
      <$> obj .: "package"
      <*> obj .: "repositoryURL"
      <*> (obj .:? "state" |?> "branch")
      <*> (obj .:? "state" |?> "revision")
      <*> (obj .:? "state" |?> "version")

-- Note, Package.resolved does not include path dependencies.
resolvedDependenciesOf :: SwiftPackageResolvedFile -> [Dependency]
resolvedDependenciesOf resolvedContent = map toDependency $ pinnedPackages resolvedContent
  where
    toDependency :: SwiftResolvedPackage -> Dependency
    toDependency pkg =
      Dependency
        { dependencyType = GitType
        , dependencyName = repositoryURL pkg
        , dependencyVersion =
            CEq
              <$> asum
                [ repositoryRevision pkg
                , repositoryVersion pkg
                , repositoryBranch pkg
                ]
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = mempty
        }
