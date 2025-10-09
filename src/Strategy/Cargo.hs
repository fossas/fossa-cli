{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

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

import App.Fossa.Analyze.LicenseAnalyze (
  LicenseAnalyzeProject (licenseAnalyzeProject),
 )
import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  ToDiagnostic,
  context,
  errCtx,
  fatalText,
  run,
  warn,
 )
import Control.Effect.Reader (Reader)
import Control.Monad (unless)
import Data.Aeson.Types (
  FromJSON (parseJSON),
  ToJSON,
  withObject,
  (.:),
  (.:?),
 )
import Data.Bifunctor (first)
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Set (Set)
import Data.String.Conversion (toText)
import Data.Text (Text, breakOn)
import Data.Text qualified as Text
import Data.Void (Void)
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
import Effect.ReadFS (ReadFS, doesFileExist, readContentsToml)
import Errata (Errata (..))
import GHC.Generics (Generic)
import Graphing (Graphing, stripRoot)
import Path (Abs, Dir, File, Path, mkRelFile, parent, parseRelFile, toFilePath, (</>))
import Text.Megaparsec (
  Parsec,
  choice,
  errorBundlePretty,
  lookAhead,
  optional,
  parse,
  takeRest,
  takeWhile1P,
  try,
 )
import Text.Megaparsec.Char (char, digitChar, space)
import Toml.Schema qualified
import Types (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (CargoType, UnresolvedPathType),
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
  analyzeProjectStaticOnly _ = const $ fatalText "Cannot analyze Cargo project statically."

data CargoPackage = CargoPackage
  { license :: Maybe Text.Text
  , cargoLicenseFile :: Maybe FilePath
  -- ^ Path relative to Cargo.toml containing the license
  }
  deriving (Eq, Show)

instance Toml.Schema.FromValue CargoPackage where
  fromValue =
    Toml.Schema.parseTableFromValue $
      CargoPackage
        <$> Toml.Schema.optKey "license"
        <*> Toml.Schema.optKey "license-file"

-- | Representation of a Cargo.toml file. See
--  [here](https://doc.rust-lang.org/cargo/reference/manifest.html)
--  for a description of this format.
newtype CargoToml = CargoToml
  {cargoPackage :: CargoPackage}
  deriving (Eq, Show)

instance Toml.Schema.FromValue CargoToml where
  fromValue =
    Toml.Schema.parseTableFromValue $
      CargoToml
        <$> Toml.Schema.reqKey "package"

instance LicenseAnalyzeProject CargoProject where
  licenseAnalyzeProject = analyzeLicenses . cargoToml

-- | Analyze a Cargo.toml for license information. The format is documented
--  (here)[https://doc.rust-lang.org/cargo/reference/manifest.html#the-license-and-license-file-fields]
analyzeLicenses :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m [LicenseResult]
analyzeLicenses tomlPath = do
  pkg <- cargoPackage <$> readContentsToml tomlPath
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

getDeps :: (Has Exec sig m, Has Diagnostics sig m, Has ReadFS sig m) => CargoProject -> m DependencyResults
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
  ( Has Exec sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  CargoProject ->
  m (Graphing Dependency, GraphBreadth)
analyze (CargoProject manifestDir manifestFile) = do
  exists <- doesFileExist $ manifestDir </> $(mkRelFile "Cargo.lock")
  unless exists $
    void $
      context "Generating lockfile" $
        errCtx (FailedToGenLockFile manifestFile) $
          execThrow manifestDir cargoGenLockfileCmd
  meta <- errCtx (FailedToRetrieveCargoMetadata manifestFile) $ execJson @CargoMetadata manifestDir cargoMetadataCmd
  graph <- context "Building dependency graph" $ pure (buildGraph meta)
  pure (graph, Complete)

newtype FailedToGenLockFile = FailedToGenLockFile (Path Abs File)
instance ToDiagnostic FailedToGenLockFile where
  renderDiagnostic (FailedToGenLockFile path) = do
    let header = "Could not generate lock file for cargo manifest: " <> toText path
    Errata (Just header) [] Nothing

newtype FailedToRetrieveCargoMetadata = FailedToRetrieveCargoMetadata (Path Abs File)
instance ToDiagnostic FailedToRetrieveCargoMetadata where
  renderDiagnostic (FailedToRetrieveCargoMetadata path) = do
    let header = "Could not retrieve machine readable cargo metadata for: " <> toText path
    Errata (Just header) [] Nothing

toDependency :: PackageId -> Set CargoLabel -> Dependency
toDependency pkg =
  foldr
    applyLabel
    Dependency
      { dependencyType = depType
      , dependencyName = depName
      , dependencyVersion = Just $ CEq $ pkgIdVersion pkg
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  where
    applyLabel :: CargoLabel -> Dependency -> Dependency
    applyLabel (CargoDepKind env) = insertEnvironment env

    parseDepKindAndProtocol :: Text.Text -> (Text.Text, Text.Text)
    parseDepKindAndProtocol src = case breakOn "+" $ fst $ breakOn ":" src of
      (protocol, "") -> ("", protocol)
      (kind, protocol) -> (kind, protocol)

    depType = case parseDepKindAndProtocol $ pkgIdSource pkg of
      ("path", _) -> UnresolvedPathType
      (_, "file") -> UnresolvedPathType
      -- Using  a git type is probably more correct, but the git type doesn't have the ability
      -- to track the package name within a repo, and this has poor support for monorepos.
      -- ("git", _) -> GitType
      -- (_, "ssh") -> GitType
      _ -> CargoType

    depName =
      let sourceUrl = Text.drop 2 $ snd $ breakOn "//" $ pkgIdSource pkg
       in case depType of
            UnresolvedPathType -> sourceUrl
            _ -> pkgIdName pkg

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

-- | Custom Parsec type alias
type PkgSpecParser a = Parsec Void Text a

-- | Parser for pre cargo v1.77.0 package ids.
oldPkgIdParser :: PkgSpecParser PackageId
oldPkgIdParser = do
  name <- takeWhile1P (Just "Package name") (/= ' ')
  void $ char ' '
  version <- takeWhile1P (Just "Package version") (/= ' ')
  void $ char ' ' >> char '('
  source <- takeWhile1P (Just "Package source") (/= ')')
  pure $
    PackageId
      { pkgIdName = name
      , pkgIdVersion = version
      , pkgIdSource = source
      }

type PkgName = Text
type PkgVersion = Text

-- | Parser for post cargo v1.77.0 package ids
newPkgIdParser :: PkgSpecParser PackageId
newPkgIdParser = eatSpaces (try longSpec <|> simplePkgSpec')
  where
    eatSpaces m = space *> m <* space

    -- Given the fragment: adler@1.0.2
    pkgName :: PkgSpecParser (PkgName, PkgVersion)
    pkgName = do
      -- Parse: adler
      name <- takeWhile1P (Just "Package name") (`notElem` ['@', ':'])
      -- Parse: @1.0.2
      version <- optional (choice [char '@', char ':'] *> semver)
      -- It's possible to specify a name with no version, use "*" in this case.
      pure (name, fromMaybe "*" version)

    simplePkgSpec' =
      pkgName >>= \(name, version) ->
        pure
          PackageId
            { pkgIdName = name
            , pkgIdVersion = version
            , pkgIdSource = ""
            }

    -- Given the spec: registry+https://github.com/rust-lang/crates.io-index#adler@1.0.2
    longSpec :: PkgSpecParser PackageId
    longSpec = do
      -- Parse: registry+https
      sourceInit <- takeWhile1P (Just "Initial URL") (/= ':')
      -- Parse: ://github.com/rust-lang/crates.io-index
      sourceRemaining <- takeWhile1P (Just "Remaining URL") (/= '#')
      let pkgSource = sourceInit <> sourceRemaining

      -- In cases where we can't find a real name, use text after the last slash as a name.
      -- e.g. file:///path/to/my/project/bar#2.0.0 has the name 'bar'
      -- Cases of this are generally path dependencies.
      let fallbackName =
            maybe pkgSource NonEmpty.last
              . NonEmpty.nonEmpty
              . filter (/= "")
              . Text.split (== '/')
              $ sourceRemaining

      -- Parse (Optional): #adler@1.0.2
      nameVersion <- optional $ do
        void $ char '#'
        -- If there's only a version after '#', use the fallback as the name.
        ((fallbackName,) <$> semver)
          <|> pkgName

      let (name, version) = fromMaybe (fallbackName, "*") nameVersion
      pure $
        PackageId
          { pkgIdName = name
          , pkgIdVersion = version
          , pkgIdSource = pkgSource
          }

    -- In the grammar, a semver always appears at the end of a string and is the only
    -- non-terminal that starts with a digit, so don't bother parsing internally.
    semver = try (lookAhead digitChar) *> takeRest

-- Prior to Cargo 1.77.0, package IDs looked like this:
-- package version (source URL)
-- adler 1.0.2 (registry+https://github.com/rust-lang/crates.io-index)
--
-- For 1.77.0 and later, they look like this:
-- registry source URL with a fragment of package@version
-- registry+https://github.com/rust-lang/crates.io-index#adler@1.0.2
-- or
-- path source URL with a fragment of package@version
-- path+file:///Users/scott/projects/health-data/health_data#package_name@0.1.0
-- or
-- path source URL with a fragment of version
-- In this case we grab the last entry in the path to use for the package name
-- path+file:///Users/scott/projects/health-data/health_data#0.1.0
--
-- Package Spec: https://doc.rust-lang.org/cargo/reference/pkgid-spec.html
parsePkgId :: MonadFail m => Text.Text -> m PackageId
parsePkgId t = either fail pure $ first errorBundlePretty parseResult
  where
    parseResult = parse (try oldPkgIdParser <|> try newPkgIdParser) "Cargo package spec" t
