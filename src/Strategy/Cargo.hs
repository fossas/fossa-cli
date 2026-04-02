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

  -- * for testing
  Package (..),
  extractGitCommitHash,
  parseGitRepoUrl,
  parsePkgId,
) where

import App.Fossa.Analyze.LicenseAnalyze (
  LicenseAnalyzeProject (licenseAnalyzeProject),
 )
import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import App.Fossa.Config.Analyze (StrategyConfig (useGitBackedCargoLocators), UseGitBackedCargoLocators (..))
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
import Control.Effect.Reader (Reader, ask)
import Control.Monad (guard, unless)
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
import Data.String.Conversion (toString, toText)
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
import Graphing (Graphing, shrinkRoots)
import Network.URI (parseURI, uriAuthority, uriPath, uriRegName)
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
import Text.Megaparsec.Char (char, digitChar, space, string)
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
  , pkgSourceUrl :: Maybe Text.Text
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
      <*> obj .:? "source"

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

getDeps :: (Has Exec sig m, Has Diagnostics sig m, Has ReadFS sig m, Has (Reader StrategyConfig) sig m) => CargoProject -> m DependencyResults
getDeps project = do
  strategyCfg <- ask @StrategyConfig
  (graph, graphBreadth) <- context "Cargo" . context "Dynamic analysis" $ analyze (unUseGitBackedCargoLocators $ useGitBackedCargoLocators strategyCfg) project
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
    , cmdEnvVars = Map.empty
    }

cargoMetadataCmd :: Command
cargoMetadataCmd =
  Command
    { cmdName = "cargo"
    , cmdArgs = ["metadata"]
    , cmdAllowErr = Never
    , cmdEnvVars = Map.empty
    }

analyze ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Bool ->
  CargoProject ->
  m (Graphing Dependency, GraphBreadth)
analyze emitGitBackedLocators (CargoProject manifestDir manifestFile) = do
  exists <- doesFileExist $ manifestDir </> $(mkRelFile "Cargo.lock")
  unless exists $
    void $
      context "Generating lockfile" $
        errCtx (FailedToGenLockFile manifestFile) $
          execThrow manifestDir cargoGenLockfileCmd
  meta <- errCtx (FailedToRetrieveCargoMetadata manifestFile) $ execJson @CargoMetadata manifestDir cargoMetadataCmd
  graph <- context "Building dependency graph" $ pure (buildGraph emitGitBackedLocators meta)
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

type PackageIdSourceKind = Text.Text
type PackageIdSourceProtocol = Text.Text

-- | Extract the git repository host+path from a cargo source URL.
-- Input:  "git+https://github.com/fossas/locator-rs?tag=v3.0.3#54c..."
-- Output: Just "github.com/fossas/locator-rs"
parseGitRepoUrl :: Text -> Maybe Text
parseGitRepoUrl src = do
  stripped <- Text.stripPrefix "git+" src
  uri <- parseURI (toString stripped)
  auth <- uriAuthority uri
  let host = toText (uriRegName auth)
      rawPath = Text.dropWhile (== '/') (toText (uriPath uri))
      cleanPath = fromMaybe rawPath (Text.stripSuffix ".git" rawPath)
  guard (not (Text.null host) && not (Text.null cleanPath))
  pure (host <> "/" <> cleanPath)

-- | Extract the commit hash from a cargo package source URL.
-- Input:  "git+https://github.com/fossas/foundation-libs#4bc3762e73f371717566fb075d02e1d25b21146e"
-- Output: Just "4bc3762e73f371717566fb075d02e1d25b21146e"
extractGitCommitHash :: Text -> Maybe Text
extractGitCommitHash src = do
  guard (Text.isPrefixOf "git+" src)
  let (_, fragment) = breakOn "#" src
  commit <- Text.stripPrefix "#" fragment
  guard (not (Text.null commit))
  pure commit

-- | A map from PackageId to the package's source URL (which contains the commit hash for git deps).
type PackageSourceMap = Map.Map PackageId Text

-- | Build a lookup from package ID to source URL from the packages list.
buildPackageSourceMap :: [Package] -> PackageSourceMap
buildPackageSourceMap = Map.fromList . concatMap toEntry
  where
    toEntry pkg = case pkgSourceUrl pkg of
      Just src -> [(pkgId pkg, src)]
      Nothing -> []

toDependency :: Bool -> PackageSourceMap -> PackageId -> Set CargoLabel -> Dependency
toDependency emitGitBackedLocators sourceMap pkg =
  foldr
    applyLabel
    Dependency
      { dependencyType = depType
      , dependencyName = depName
      , dependencyVersion = Just $ CEq depVersion
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = Map.empty
      }
  where
    applyLabel :: CargoLabel -> Dependency -> Dependency
    applyLabel (CargoDepKind env) = insertEnvironment env

    -- For example:
    -- path+file:///some/file/path -> ("path", "file")
    -- file:///some/file/path      -> ("", "file")
    parseDepKindAndProtocol :: Text.Text -> (PackageIdSourceKind, PackageIdSourceProtocol)
    parseDepKindAndProtocol src = case breakOn "+" . Text.takeWhile (/= ':') $ src of
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

    -- For a path dependency, use the path as the package name. For example:
    -- path+file:///some/file/path -> /some/file/path
    -- For a git dependency when the server supports it, use repo-url#crate-name. For example:
    -- git+https://github.com/fossas/locator-rs?tag=v3.0.3#sha -> github.com/fossas/locator-rs#locator
    -- When the server does not support git-backed locators, fall back to the plain crate name.
    depName =
      let sourceUrl = Text.drop 2 $ snd $ breakOn "//" $ pkgIdSource pkg
       in case depType of
            UnresolvedPathType -> sourceUrl
            _
              | emitGitBackedLocators
              , Just repoUrl <- parseGitRepoUrl (pkgIdSource pkg) ->
                  repoUrl <> "#" <> pkgIdName pkg
            _ -> pkgIdName pkg

    -- For git dependencies without a tag, use the commit hash from the package source URL.
    -- For all other dependencies (including tagged git deps), use the crate version.
    depVersion = case (emitGitBackedLocators, untaggedGitCommitHash) of
      (True, Just commitHash) -> commitHash
      _ -> pkgIdVersion pkg

    -- Look up the commit hash for an untagged git dependency.
    -- Note: the `git+` here is from a URL like `git+https://github.com...`, not from a git+ locator.
    untaggedGitCommitHash :: Maybe Text
    untaggedGitCommitHash = case ("git+" `Text.isPrefixOf` pkgIdSource pkg, "?tag=" `Text.isInfixOf` pkgIdSource pkg) of
      (True, False) -> do
        sourceUrl <- Map.lookup pkg sourceMap
        extractGitCommitHash sourceUrl
      _ -> Nothing

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

buildGraph :: Bool -> CargoMetadata -> Graphing Dependency
-- By construction, workspace members are the root nodes in the graph.
-- Use shrinkRoots to remove them and promote their direct dependencies to the
-- direct dependencies we report for the project.
buildGraph emitGitBackedLocators meta = shrinkRoots $
  run . withLabeling (toDependency emitGitBackedLocators sourceMap) $ do
    traverse_ direct $ metadataWorkspaceMembers meta
    traverse_ addEdge $ resolvedNodes $ metadataResolve meta
  where
    sourceMap = buildPackageSourceMap $ metadataPackages meta

-- | Custom Parsec type alias
type PkgSpecParser a = Parsec Void Text a

-- | Parser for pre cargo v1.77.0 package ids.
oldPkgIdParser :: PkgSpecParser PackageId
oldPkgIdParser = do
  name <- takeWhile1P (Just "Package name") (/= ' ') <* char ' '
  version <- takeWhile1P (Just "Package version") (/= ' ') <* string " ("
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
      -- Strip query parameters (e.g. ?tag=v0.3.6) before splitting, so that
      -- git+https://github.com/fossas/broker?tag=v0.3.6#0.3.6 yields "broker", not "broker?tag=v0.3.6".
      let fallbackName =
            maybe pkgSource NonEmpty.last
              . NonEmpty.nonEmpty
              . filter (/= "")
              . Text.split (== '/')
              $ Text.takeWhile (/= '?') sourceRemaining

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
    parseResult = parse (try oldPkgIdParser <|> newPkgIdParser) "Cargo package spec" t
