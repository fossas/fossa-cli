module Strategy.Swift.PackageResolved (
  SwiftPackageResolvedFile (..),
  SwiftResolvedPackage (..),
  resolvedDependenciesOf,
) where

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
import Data.Text (Text)
import DepTypes (DepType (GitType), Dependency (..), VerConstraint (CEq))

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

instance FromJSON SwiftPackageResolvedFile where
  parseJSON = withObject "Package.resolved content" $ \obj -> do
    version :: Integer <- obj .: "version"
    case version of
      1 -> SwiftPackageResolvedFile version <$> ((obj .: "object" |> "pins") >>= traverse (withObject "Package.resolved v1 pin" parseV1Pin))
      2 -> SwiftPackageResolvedFile version <$> ((obj .: "pins") >>= traverse (withObject "Package.resolved v2 pin" parseV2Pin))
      _ -> fail $ "unknown Package.resolved version: " <> show version

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

parseV1Pin :: Object -> Parser SwiftResolvedPackage
parseV1Pin obj =
  SwiftResolvedPackage
    <$> obj .: "package"
    <*> obj .: "repositoryURL"
    <*> (obj .:? "state" |?> "branch")
    <*> (obj .:? "state" |?> "revision")
    <*> (obj .:? "state" |?> "version")

parseV2Pin :: Object -> Parser SwiftResolvedPackage
parseV2Pin obj =
  SwiftResolvedPackage
    <$> obj .: "identity"
    <*> obj .: "location"
    <*> pure Nothing
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
