module Strategy.Cargo (
  discover,
  CargoMetadata (..),
  CargoProject (..),
  NodeDependency (..),
  NodeDepKind (..),
  PackageId (..),
  Resolve (..),
  ResolveNode (..),
  buildGraph,
  getDeps,
  mkProject,
  findProjects,
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import App.Pathfinder.Types (LicenseAnalyzeProject (licenseAnalyzeProject))
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  ToDiagnostic,
  context,
  errCtx,
  run,
  warn,
 )
import Control.Effect.Reader (Reader)
import Data.Aeson.Types (
  FromJSON (parseJSON),
  Parser,
  ToJSON,
  withObject,
  (.:),
  (.:?),
 )
import Data.Foldable (for_, traverse_)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust)
import Data.Set (Set)
import Data.String.Conversion (toText)
import Data.Text qualified as Text
import Diag.Diagnostic (renderDiagnostic)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipAll),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  execJson,
  execThrow,
 )
import Effect.Grapher (
  LabeledGrapher,
  direct,
  edge,
  label,
  withLabeling,
 )
import Effect.ReadFS (ReadFS, readContentsToml)
import GHC.Generics (Generic)
import Graphing (Graphing, stripRoot)
import Path (Abs, Dir, File, Path, parent, parseRelFile, toFilePath, (</>))
import Prettyprinter (Pretty (pretty))
import Toml (TomlCodec, dioptional, diwrap, (.=))
import Toml qualified
import Types (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (CargoType),
  Dependency (..),
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (CargoProjectType),
  GraphBreadth (Complete),
  License (License),
  LicenseResult (LicenseResult, licenseFile, licensesFound),
  LicenseType (LicenseFile, LicenseSPDX, UnknownType),
  VerConstraint (CEq),
  insertEnvironment,
 )

newtype CargoLabel
  = CargoDepKind DepEnvironment
  deriving (Eq, Ord, Show)

data PackageId = PackageId
  { pkgIdName :: Text.Text
  , pkgIdVersion :: Text.Text
  , pkgIdSource :: Text.Text
  }
  deriving (Eq, Ord, Show)

data PackageDependency = PackageDependency
  { pkgDepName :: Text.Text
  , pkgDepReq :: Text.Text
  , pkgDepKind :: Maybe Text.Text
  }
  deriving (Eq, Ord, Show)

data Package = Package
  { pkgName :: Text.Text
  , pkgVersion :: Text.Text
  , pkgId :: PackageId
  , pkgLicense :: Maybe Text.Text
  , pkgLicenseFile :: Maybe Text.Text
  , pkgDependencies :: [PackageDependency]
  }
  deriving (Eq, Ord, Show)

data NodeDepKind = NodeDepKind
  { nodeDepKind :: Maybe Text.Text
  , nodeDepTarget :: Maybe Text.Text
  }
  deriving (Eq, Ord, Show)

data NodeDependency = NodeDependency
  { nodePkg :: PackageId
  , nodeDepKinds :: [NodeDepKind]
  }
  deriving (Eq, Ord, Show)

data ResolveNode = ResolveNode
  { resolveNodeId :: PackageId
  , resolveNodeDeps :: [NodeDependency]
  }
  deriving (Eq, Ord, Show)

newtype Resolve = Resolve
  { resolvedNodes :: [ResolveNode]
  }
  deriving (Eq, Ord, Show)

data CargoMetadata = CargoMetadata
  { metadataPackages :: [Package]
  , metadataWorkspaceMembers :: [PackageId]
  , metadataResolve :: Resolve
  }
  deriving (Eq, Ord, Show)

instance FromJSON PackageDependency where
  parseJSON = withObject "PackageDependency" $ \obj ->
    PackageDependency
      <$> obj .: "name"
      <*> obj .: "req"
      <*> obj .:? "kind"

instance FromJSON Package where
  parseJSON = withObject "Package" $ \obj ->
    Package
      <$> obj .: "name"
      <*> obj .: "version"
      <*> (obj .: "id" >>= parsePkgId)
      <*> obj .:? "license"
      <*> obj .:? "license_file"
      <*> obj .: "dependencies"

instance FromJSON NodeDepKind where
  parseJSON = withObject "NodeDepKind" $ \obj ->
    NodeDepKind
      <$> obj .:? "kind"
      <*> obj .:? "target"

instance FromJSON NodeDependency where
  parseJSON = withObject "NodeDependency" $ \obj ->
    NodeDependency
      <$> (obj .: "pkg" >>= parsePkgId)
      <*> obj .: "dep_kinds"

instance FromJSON ResolveNode where
  parseJSON = withObject "ResolveNode" $ \obj ->
    ResolveNode
      <$> (obj .: "id" >>= parsePkgId)
      <*> obj .: "deps"

instance FromJSON Resolve where
  parseJSON = withObject "Resolve" $ \obj ->
    Resolve <$> obj .: "nodes"

instance FromJSON CargoMetadata where
  parseJSON = withObject "CargoMetadata" $ \obj ->
    CargoMetadata
      <$> obj .: "packages"
      <*> (obj .: "workspace_members" >>= traverse parsePkgId)
      <*> obj .: "resolve"

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject CargoProject]
discover = simpleDiscover findProjects mkProject CargoProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [CargoProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "Cargo.toml" files of
    Nothing -> pure ([], WalkContinue)
    Just toml -> do
      let project =
            CargoProject
              { cargoToml = toml
              , cargoDir = dir
              }

      pure ([project], WalkSkipAll)

data CargoProject = CargoProject
  { cargoDir :: Path Abs Dir
  , cargoToml :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON CargoProject

instance AnalyzeProject CargoProject where
  analyzeProject _ = getDeps

data CargoPackage = CargoPackage
  { license :: Maybe Text.Text
  , cargoLicenseFile :: Maybe FilePath
  -- ^ Path relative to Cargo.toml containing the license
  }
  deriving (Eq, Show)

cargoPackageCodec :: TomlCodec CargoPackage
cargoPackageCodec =
  CargoPackage
    <$> dioptional (Toml.text "license") .= license
    <*> dioptional (Toml.string "license-file") .= cargoLicenseFile

-- |Representation of a Cargo.toml file. See
-- [here](https://doc.rust-lang.org/cargo/reference/manifest.html)
-- for a description of this format.
newtype CargoToml = CargoToml
  {cargoPackage :: CargoPackage}
  deriving (Eq, Show)

cargoTomlCodec :: TomlCodec CargoToml
cargoTomlCodec = diwrap (Toml.table cargoPackageCodec "package")
-- ^^ The above is a bit obscure. It's generating a TomlCodec CargoPackage and
-- then using 'diwrap'/Coercible to make a TomlCodec CargoToml.  I can't use
-- 'CargoToml <$>' because TomlCodec aliases (Codec a a) and only (Codec a)
-- has a Functor instance, so I'd end up with a (Codec CargoPackage CargoToml).

instance LicenseAnalyzeProject CargoProject where
  licenseAnalyzeProject = analyzeLicenses . cargoToml

-- |Analyze a Cargo.toml for license information. The format is documented
-- (here)[https://doc.rust-lang.org/cargo/reference/manifest.html#the-license-and-license-file-fields]
analyzeLicenses :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m [LicenseResult]
analyzeLicenses tomlPath = do
  pkg <- cargoPackage <$> readContentsToml cargoTomlCodec tomlPath
  licensePathText <- maybe (pure Nothing) mkLicensePath (cargoLicenseFile pkg)

  -- The license-file field in Cargo.toml is relative to the dir of the
  -- Cargo.toml file. Generate an absolute path to license-file.
  let maybeLicense = license pkg
  let licenseCon = selectLicenseCon <$> maybeLicense
  pure
    [ LicenseResult
        { licenseFile = toFilePath tomlPath
        , licensesFound =
            catMaybes
              [ License <$> licenseCon <*> maybeLicense
              , License LicenseFile <$> licensePathText
              ]
        }
    ]
  where
    mkLicensePath path = case parseRelFile path of
      Just p -> pure . Just . toText $ parent tomlPath </> p
      Nothing ->
        warn ("Cannot parse 'license-file' value: " <> path)
          >> pure Nothing

    textElem c = isJust . Text.findIndex (== c)
    -- Old versions of Cargo allow '/' as a separator between SPDX values in
    -- 'license'. In that case 'license' can't be treated as a LicenseSPDX.
    selectLicenseCon licenseText =
      if textElem '/' licenseText
        then UnknownType
        else LicenseSPDX

mkProject :: CargoProject -> DiscoveredProject CargoProject
mkProject project =
  DiscoveredProject
    { projectType = CargoProjectType
    , projectBuildTargets = mempty
    , projectPath = cargoDir project
    , projectData = project
    }

getDeps :: (Has Exec sig m, Has Diagnostics sig m) => CargoProject -> m DependencyResults
getDeps project = do
  (graph, graphBreadth) <- context "Cargo" . context "Dynamic analysis" . analyze $ project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [cargoToml project]
      }

cargoGenLockfileCmd :: Command
cargoGenLockfileCmd =
  Command
    { cmdName = "cargo"
    , cmdArgs = ["generate-lockfile"]
    , cmdAllowErr = Never
    }

cargoMetadataCmd :: Command
cargoMetadataCmd =
  Command
    { cmdName = "cargo"
    , cmdArgs = ["metadata"]
    , cmdAllowErr = Never
    }

analyze ::
  (Has Exec sig m, Has Diagnostics sig m) =>
  CargoProject ->
  m (Graphing Dependency, GraphBreadth)
analyze (CargoProject manifestDir manifestFile) = do
  _ <- context "Generating lockfile" $ errCtx (FailedToGenLockFile manifestFile) $ execThrow manifestDir cargoGenLockfileCmd
  meta <- errCtx (FailedToRetrieveCargoMetadata manifestFile) $ execJson @CargoMetadata manifestDir cargoMetadataCmd
  graph <- context "Building dependency graph" $ pure (buildGraph meta)
  pure (graph, Complete)

newtype FailedToGenLockFile = FailedToGenLockFile (Path Abs File)
instance ToDiagnostic FailedToGenLockFile where
  renderDiagnostic (FailedToGenLockFile path) = pretty $ "Could not generate lock file for cargo manifest: " <> (show path)

newtype FailedToRetrieveCargoMetadata = FailedToRetrieveCargoMetadata (Path Abs File)
instance ToDiagnostic FailedToRetrieveCargoMetadata where
  renderDiagnostic (FailedToRetrieveCargoMetadata path) = pretty $ "Could not retrieve machine readable cargo metadata for: " <> (show path)

toDependency :: PackageId -> Set CargoLabel -> Dependency
toDependency pkg =
  foldr
    applyLabel
    Dependency
      { dependencyType = CargoType
      , dependencyName = pkgIdName pkg
      , dependencyVersion = Just $ CEq $ pkgIdVersion pkg
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  where
    applyLabel :: CargoLabel -> Dependency -> Dependency
    applyLabel (CargoDepKind env) = insertEnvironment env

-- Possible values here are "build", "dev", and null.
-- Null refers to productions, while dev and build refer to development-time dependencies
-- Cargo does not differentiate test dependencies and dev dependencies,
-- so we just simplify it to Development.
kindToLabel :: Maybe Text.Text -> CargoLabel
kindToLabel (Just _) = CargoDepKind EnvDevelopment
kindToLabel Nothing = CargoDepKind EnvProduction

addLabel :: Has (LabeledGrapher PackageId CargoLabel) sig m => NodeDependency -> m ()
addLabel dep = do
  let packageId = nodePkg dep
  traverse_ (label packageId . kindToLabel . nodeDepKind) $ nodeDepKinds dep

addEdge :: Has (LabeledGrapher PackageId CargoLabel) sig m => ResolveNode -> m ()
addEdge node = do
  let parentId = resolveNodeId node
  for_ (resolveNodeDeps node) $ \dep -> do
    addLabel dep
    edge parentId $ nodePkg dep

buildGraph :: CargoMetadata -> Graphing Dependency
buildGraph meta = stripRoot $
  run . withLabeling toDependency $ do
    traverse_ direct $ metadataWorkspaceMembers meta
    traverse_ addEdge $ resolvedNodes $ metadataResolve meta

parsePkgId :: Text.Text -> Parser PackageId
parsePkgId t =
  case Text.splitOn " " t of
    [a, b, c] -> pure $ PackageId a b c
    _ -> fail "malformed Package ID"
