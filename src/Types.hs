{-# LANGUAGE RecordWildCards #-}

module Types (
  ArchiveUploadType (..),
  DiscoveredProject (..),
  DependencyResults (..),
  GraphBreadth (..),
  FoundTargets (..),
  BuildTarget (..),
  LicenseResult (..),
  License (..),
  LicenseType (..),
  module DepTypes,
  TargetFilter (..),
  DiscoveredProjectType (..),
  projectTypeToText,
) where

import Data.Aeson (
  FromJSON (parseJSON),
  KeyValue ((.=)),
  ToJSON (toEncoding, toJSON),
  defaultOptions,
  genericToEncoding,
  object,
  withObject,
  withText,
  (.:),
  (.:?),
 )
import Data.Aeson.Types (Parser)
import Data.Set.NonEmpty (NonEmptySet)
import Data.String.Conversion (ToText (toText), toString)
import Data.Text (Text)
import DepTypes (
  DepEnvironment (..),
  DepType (..),
  Dependency (..),
  VerConstraint (..),
  insertEnvironment,
  insertLocation,
  insertTag,
 )
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, Dir, File, Path, Rel, parseRelDir)
import Prettyprinter (Pretty (pretty))

-- TODO: results should be within a graph of build targets && eliminate SubprojectType
data FoundTargets = ProjectWithoutTargets | FoundTargets (NonEmptySet BuildTarget)
  deriving (Eq, Ord, Show, Generic)

instance ToJSON FoundTargets

instance Semigroup FoundTargets where
  a <> ProjectWithoutTargets = a
  ProjectWithoutTargets <> a = a
  (FoundTargets a) <> (FoundTargets b) = FoundTargets (a <> b)

instance Monoid FoundTargets where
  mempty = ProjectWithoutTargets

data DiscoveredProjectType
  = AlpineDatabaseProjectType
  | BerkeleyDBProjectType
  | BinaryDepsProjectType
  | BundlerProjectType
  | CabalProjectType
  | CargoProjectType
  | CarthageProjectType
  | CocoapodsProjectType
  | ComposerProjectType
  | CondaProjectType
  | DpkgDatabaseProjectType
  | FpmProjectType
  | GlideProjectType
  | GodepProjectType
  | GomodProjectType
  | GradleProjectType
  | LeiningenProjectType
  | MavenProjectType
  | MixProjectType
  | NDBProjectType
  | NimbleProjectType
  | NpmProjectType
  | NuspecProjectType
  | PackageReferenceProjectType
  | PackagesConfigProjectType
  | PaketProjectType
  | PerlProjectType
  | PipenvProjectType
  | PnpmProjectType
  | PoetryProjectType
  | ProjectAssetsJsonProjectType
  | ProjectJsonProjectType
  | PubProjectType
  | Rebar3ProjectType
  | RepoManifestProjectType
  | RpmProjectType
  | ScalaProjectType
  | SetuptoolsProjectType
  | StackProjectType
  | SwiftProjectType
  | VsiProjectType
  | YarnProjectType
  deriving (Eq, Ord, Show, Enum, Bounded)

projectTypeToText :: DiscoveredProjectType -> Text
projectTypeToText = \case
  AlpineDatabaseProjectType -> "apkdb"
  BerkeleyDBProjectType -> "berkeleydb"
  BinaryDepsProjectType -> "binary-deps"
  BundlerProjectType -> "bundler"
  CabalProjectType -> "cabal"
  CargoProjectType -> "cargo"
  CarthageProjectType -> "carthage"
  CocoapodsProjectType -> "cocoapods"
  ComposerProjectType -> "composer"
  CondaProjectType -> "conda"
  DpkgDatabaseProjectType -> "dpkgdb"
  FpmProjectType -> "fpm"
  GlideProjectType -> "glide"
  GodepProjectType -> "godep"
  GomodProjectType -> "gomod"
  GradleProjectType -> "gradle"
  LeiningenProjectType -> "leiningen"
  MavenProjectType -> "maven"
  MixProjectType -> "mix"
  NDBProjectType -> "ndb"
  NimbleProjectType -> "nimble"
  NpmProjectType -> "npm"
  NuspecProjectType -> "nuspec"
  PackageReferenceProjectType -> "packagereference"
  PackagesConfigProjectType -> "packagesconfig"
  PaketProjectType -> "packet"
  PerlProjectType -> "perl"
  PipenvProjectType -> "pipenv"
  PnpmProjectType -> "pnpm"
  PoetryProjectType -> "poetry"
  ProjectAssetsJsonProjectType -> "projectassetsjson"
  ProjectJsonProjectType -> "projectjson"
  PubProjectType -> "pub"
  Rebar3ProjectType -> "rebar3"
  RepoManifestProjectType -> "repomanifest"
  RpmProjectType -> "rpm"
  ScalaProjectType -> "scala"
  SetuptoolsProjectType -> "setuptools"
  StackProjectType -> "stack"
  SwiftProjectType -> "swift"
  VsiProjectType -> "vsi"
  YarnProjectType -> "yarn"

instance ToJSON DiscoveredProjectType where
  toJSON = toJSON . toText

instance ToText DiscoveredProjectType where
  toText = projectTypeToText

instance Pretty DiscoveredProjectType where
  pretty = pretty . toText

-- | A project found during project discovery, parameterized by the monad
-- used to perform dependency analysis
data DiscoveredProject a = DiscoveredProject
  { projectType :: DiscoveredProjectType
  , projectPath :: Path Abs Dir
  , projectBuildTargets :: FoundTargets
  , projectData :: a
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON a => ToJSON (DiscoveredProject a)

-- | The results from analyzing dependencies on a project. This contains the graph,
-- the GraphBreadth (if it was partially analyzed, or fully analyzed), and the
-- manifest file(s) (if available) used to determine the graph
data DependencyResults = DependencyResults
  { dependencyGraph :: Graphing Dependency
  , dependencyGraphBreadth :: GraphBreadth
  , dependencyManifestFiles :: [Path Abs File]
  }
  deriving (Show, Eq)

-- | The exhaustiveness or completeness of the graph found during analysis.
--   Complete - indicates that the dependencies in question are a full, transitive graph, requiring no additional analysis
--      Ex -> yarn.lock, Podfile.lock, Gemfile.lock
--   Partial - indicates that the dependencies in question do *NOT* represent the fully resolved graph, i.e. because of a limitation
--             in the package manager in use, it wasn't possible for us to obtain a full list of deps. This will signal to Core that
--             further analysis may need to be run to determine the full graph.
--      Ex -> stand-alone `package.json`, stand-alone Podfile
data GraphBreadth = Complete | Partial
  deriving (Eq, Ord, Show)

instance FromJSON GraphBreadth where
  parseJSON = withText "GraphBreadth" $ \case
    "complete" -> pure Complete
    "partial" -> pure Partial
    _ -> fail "invalid GraphBreadth value"

instance ToJSON GraphBreadth where
  -- render as text
  toJSON = toJSON . renderGraphType
    where
      renderGraphType :: GraphBreadth -> Text
      renderGraphType = \case
        Complete -> "complete"
        Partial -> "partial"

newtype BuildTarget = BuildTarget {unBuildTarget :: Text}
  deriving (Eq, Ord, Show, ToJSON)

{-
  The following filters separate the difference between the following filters:
    gomod -> TypeTarget
    mvn@foo/ -> TypeDirTarget
    gradle@./::test-benchmark -> TypeDirTargetTarget

  The majority of build targets consist of a strategy type and a directory.
  However, many Gradle targets consist of a strategy type, a directory,
  and an exact gradle target.
-}
data TargetFilter
  = TypeTarget Text
  | TypeDirTarget Text (Path Rel Dir)
  | TypeDirTargetTarget Text (Path Rel Dir) BuildTarget
  deriving (Eq, Ord, Show, Generic)

instance ToJSON TargetFilter where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TargetFilter where
  parseJSON = withObject "TargetFilter" $ \obj -> do
    tool <- obj .: "type"
    ts <- obj .:? "path" >>= traverse pathParser
    case ts of
      Nothing -> pure $ TypeTarget tool
      Just path -> do
        targetField <- obj .:? "target"
        case targetField of
          Nothing -> pure $ TypeDirTarget tool path
          Just targetFound -> pure $ TypeDirTargetTarget tool path (BuildTarget targetFound)

pathParser :: Text -> Parser (Path Rel Dir)
pathParser input = do
  case parseRelDir (toString input) of
    Left err -> fail (show err)
    Right value -> pure value

data LicenseResult = LicenseResult
  { licenseFile :: FilePath
  , licensesFound :: [License]
  }
  deriving (Eq, Ord, Show)

data License = License
  { licenseType :: LicenseType
  , licenseValue :: Text
  }
  deriving (Eq, Ord, Show)

data LicenseType
  = LicenseURL
  | LicenseFile
  | LicenseSPDX
  | UnknownType
  deriving (Eq, Ord, Show)

instance ToJSON License where
  toJSON License{..} =
    object
      [ "type" .= textType licenseType
      , "value" .= licenseValue
      ]
    where
      textType :: LicenseType -> Text
      textType = \case
        LicenseURL -> "url"
        LicenseFile -> "file"
        LicenseSPDX -> "spdx"
        UnknownType -> "unknown"

instance ToJSON LicenseResult where
  toJSON LicenseResult{..} =
    object
      [ "filepath" .= licenseFile
      , "licenses" .= licensesFound
      ]

data ArchiveUploadType
  = ArchiveUpload
  | CLILicenseScan
  deriving (Eq, Ord, Show)

instance FromJSON ArchiveUploadType where
  parseJSON = withText "ArchiveUploadType" $ \case
    "ArchiveUpload" -> pure ArchiveUpload
    "CLILicenseScan" -> pure CLILicenseScan
    notSupported -> fail . toString $ "Expected either: 'ArchiveUpload' or 'CLILicenseScan' for scanType. You provided: " <> notSupported

instance ToJSON ArchiveUploadType where
  toJSON = toJSON . show
