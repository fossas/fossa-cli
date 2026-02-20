{-# LANGUAGE OverloadedRecordDot #-}

module Strategy.Node.Bun.BunLock (
  analyze,
  buildGraph,

  -- * for testing
  BunLockfile (..),
  BunWorkspace (..),
  BunPackage (..),
  BunPackageDeps (..),
  BunDepVertex (..),
  BunDepLabel (..),
  parseResolution,
) where

import Control.Algebra (Has)
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Aeson (
  FromJSON (parseJSON),
  Value (Object),
  withArray,
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import DepTypes (
  DepEnvironment (..),
  DepType (GitType, NodeJSType),
  Dependency (..),
  VerConstraint (CEq),
  insertEnvironment,
 )
import Effect.Grapher (LabeledGrapher, deep, direct, edge, label, run, withLabeling)
import Effect.ReadFS (ReadFS, readContentsJsonc)
import Graphing (Graphing)
import Path (Abs, File, Path)

-- | Bun lockfile (bun.lock) in JSONC format.
--
-- Implemented against lockfile version 1 (bun v1.2.x).
--
-- See @docs/references/strategies/languages/nodejs/bun.md@ for the full
-- lockfile format documentation.
data BunLockfile = BunLockfile
  { workspaces :: Map WorkspacePath BunWorkspace
  , packages :: Map PackageName BunPackage
  }
  deriving (Show, Eq)

-- | Relative path from the project root to the workspace directory.
-- The root workspace uses an empty string @""@.
type WorkspacePath = Text

-- | Package name as it appears in lockfile keys, e.g. @"lodash"@ or @"@scope/pkg"@.
type PackageName = Text

-- | Version constraint as declared in package.json, e.g. @"^4.17.21"@.
type VersionConstraint = Text

-- | Graph vertex: identifies a package without environment info.
data BunDepVertex = BunDepVertex
  { bunDepType :: DepType
  , bunDepName :: Text
  , bunDepVersion :: Maybe VerConstraint
  }
  deriving (Eq, Ord, Show)

newtype BunDepLabel = BunDepEnv DepEnvironment
  deriving (Eq, Ord, Show)

-- | Convert a vertex and its accumulated labels into a 'Dependency'.
vertexToDependency :: BunDepVertex -> Set.Set BunDepLabel -> Dependency
vertexToDependency vertex = foldr applyLabel base
  where
    base =
      Dependency
        { dependencyType = bunDepType vertex
        , dependencyName = bunDepName vertex
        , dependencyVersion = bunDepVersion vertex
        , dependencyLocations = mempty
        , dependencyEnvironments = mempty
        , dependencyTags = mempty
        }
    applyLabel (BunDepEnv env) = insertEnvironment env

data BunWorkspace = BunWorkspace
  { wsName :: PackageName
  , wsDependencies :: Map PackageName VersionConstraint
  , wsDevDependencies :: Map PackageName VersionConstraint
  , wsOptionalDependencies :: Map PackageName VersionConstraint
  }
  deriving (Show, Eq, Ord)

-- | Resolved dependencies extracted from a package's info object.
data BunPackageDeps = BunPackageDeps
  { pkgDepsDependencies :: Map PackageName VersionConstraint
  , pkgDepsOptionalDependencies :: Map PackageName VersionConstraint
  , pkgDepsPeerDependencies :: Map PackageName VersionConstraint
  }
  deriving (Show, Eq)

-- | A resolved package entry in the lockfile.
data BunPackage = BunPackage
  { pkgResolution :: Text
  -- ^ e.g. @"lodash\@4.17.21"@, @"pkg\@file:../local"@, @"pkg\@workspace:packages/a"@
  , pkgDeps :: BunPackageDeps
  -- ^ Transitive dependency info extracted from the package array.
  }
  deriving (Show, Eq)

instance FromJSON BunLockfile where
  parseJSON = withObject "BunLockfile" $ \obj ->
    BunLockfile
      <$> obj .:? "workspaces" .!= mempty
      <*> obj .:? "packages" .!= mempty

instance FromJSON BunWorkspace where
  parseJSON = withObject "BunWorkspace" $ \obj ->
    BunWorkspace
      <$> obj .:? "name" .!= ""
      <*> obj .:? "dependencies" .!= mempty
      <*> obj .:? "devDependencies" .!= mempty
      <*> obj .:? "optionalDependencies" .!= mempty

instance FromJSON BunPackageDeps where
  parseJSON = withObject "BunPackageDeps" $ \obj ->
    BunPackageDeps
      <$> obj .:? "dependencies" .!= mempty
      <*> obj .:? "optionalDependencies" .!= mempty
      <*> obj .:? "peerDependencies" .!= mempty

-- | Package arrays are variable-length. The first element is always
-- the resolution string. We find the first JSON object in the
-- remaining elements (the dependency metadata object).
instance FromJSON BunPackage where
  parseJSON = withArray "BunPackage" $ \arr ->
    case V.toList arr of
      [] -> fail "Expected non-empty package array"
      (resVal : rest) -> do
        resolution <- parseJSON resVal
        deps <- case filter isObject rest of
          (obj : _) -> parseJSON obj
          [] -> pure emptyDeps
        pure $ BunPackage resolution deps
    where
      isObject (Object _) = True
      isObject _ = False

      emptyDeps :: BunPackageDeps
      emptyDeps = BunPackageDeps mempty mempty mempty

-- | Parse a resolution string into (name, version).
--
-- >>> parseResolution "lodash@4.17.21"
-- ("lodash", "4.17.21")
--
-- >>> parseResolution "@scope/pkg@1.0.0"
-- ("@scope/pkg", "1.0.0")
--
-- >>> parseResolution "pkg@file:../local"
-- ("pkg", "file:../local")
--
-- >>> parseResolution "pkg@workspace:packages/a"
-- ("pkg", "workspace:packages/a")
parseResolution :: Text -> (Text, Text)
parseResolution res
  | "@" `Text.isPrefixOf` res =
      -- Scoped package: @scope/name@version
      let withoutAt = Text.drop 1 res
          (scopeAndName, rest) = Text.breakOn "@" withoutAt
       in ("@" <> scopeAndName, Text.drop 1 rest)
  | otherwise =
      let (name, rest) = Text.breakOn "@" res
       in (name, Text.drop 1 rest)

-- | Analyze a bun.lock file and produce a dependency graph.
analyze ::
  (Has ReadFS sig m, Has Diagnostics sig m) =>
  Path Abs File ->
  m (Graphing Dependency)
analyze file = do
  lockfile <- context "Parsing bun.lock" $ readContentsJsonc file
  context "Building dependency graph" $ pure $ buildGraph lockfile

-- | Build a dependency graph from a parsed bun lockfile.
--
-- Strategy:
--   1. Collect all dev dependency names across all workspaces.
--   2. For each workspace, mark its declared dependencies as direct
--      and label them with their environment.
--   3. For each supported package (npm, git), add it as a deep dependency,
--      label it with its inferred environment, and create edges to its
--      transitive dependencies.
--      Unsupported types (workspace, file, link, root, module) are excluded.
--
-- Uses 'LabeledGrapher' so that vertices are environment-agnostic and
-- environments accumulate as labels, avoiding duplicate vertices when
-- the same package appears in both prod and dev across workspaces.
buildGraph :: BunLockfile -> Graphing Dependency
buildGraph lockfile = run . withLabeling vertexToDependency $ do
  for_ allWorkspaces $ \workspace -> do
    markDirectDeps EnvProduction workspace.wsDependencies
    markDirectDeps EnvDevelopment workspace.wsDevDependencies
    markDirectDeps EnvProduction workspace.wsOptionalDependencies

  for_ (packages lockfile) $ \pkg ->
    for_ (toVertex pkg) $ \parentVertex -> do
      let (name, _) = parseResolution (pkgResolution pkg)
          inferredEnv = if Set.member name devDepNames then EnvDevelopment else EnvProduction
      deep parentVertex
      label parentVertex (BunDepEnv inferredEnv)
      for_ (transitiveDepNames pkg) $ \childName ->
        case Map.lookup childName (packages lockfile) of
          Nothing -> pure ()
          Just childPkg ->
            for_ (toVertex childPkg) $ \childVertex ->
              edge parentVertex childVertex
  where
    allWorkspaces :: [BunWorkspace]
    allWorkspaces = Map.elems $ workspaces lockfile

    devDepNames :: Set.Set PackageName
    devDepNames = Set.fromList $ concatMap (Map.keys . wsDevDependencies) allWorkspaces

    markDirectDeps :: (Has (LabeledGrapher BunDepVertex BunDepLabel) sig m) => DepEnvironment -> Map PackageName VersionConstraint -> m ()
    markDirectDeps env deps =
      for_ (Map.keys deps) $ \depName ->
        case Map.lookup depName (packages lockfile) of
          Nothing -> pure ()
          Just pkg -> for_ (toVertex pkg) $ \vertex -> do
            direct vertex
            label vertex (BunDepEnv env)

    transitiveDepNames :: BunPackage -> [PackageName]
    transitiveDepNames pkg =
      Map.keys (pkgDepsDependencies $ pkgDeps pkg)
        <> Map.keys (pkgDepsOptionalDependencies $ pkgDeps pkg)
        <> Map.keys (pkgDepsPeerDependencies $ pkgDeps pkg)

    -- | Convert a package to a vertex (environment-agnostic).
    --
    -- Returns Nothing for unsupported resolution types (workspace, file, link,
    -- tarball, root, module). Only npm and git/github packages are included.
    toVertex :: BunPackage -> Maybe BunDepVertex
    toVertex pkg =
      let (name, version) = parseResolution (pkgResolution pkg)
       in resolutionToVertex name version

    -- | Convert a parsed resolution to a vertex based on the version prefix.
    -- Only npm (no prefix) and git resolutions produce vertices.
    resolutionToVertex :: Text -> Text -> Maybe BunDepVertex
    resolutionToVertex name version = case stripGitPrefix version of
      Just ref -> Just $ mkGitVertex ref
      Nothing
        | isUnsupportedRef version -> Nothing
        | otherwise -> Just $ mkVertex NodeJSType name version

    -- | Strip a known git hosting prefix from a version string.
    stripGitPrefix :: Text -> Maybe Text
    stripGitPrefix v =
      Text.stripPrefix "github:" v
        <|> Text.stripPrefix "gitlab:" v
        <|> Text.stripPrefix "bitbucket:" v
        <|> Text.stripPrefix "git+" v

    -- | Build a GitType vertex from a git reference like @"user/repo#ref"@
    -- or @"https://github.com/user/repo.git#ref"@.
    mkGitVertex :: Text -> BunDepVertex
    mkGitVertex ref =
      let (repo, refPart) = Text.breakOn "#" ref
       in mkVertex GitType repo (Text.drop 1 refPart)

    -- | Check if a version string refers to a local/unsupported resolution type.
    isUnsupportedRef :: Text -> Bool
    isUnsupportedRef v =
      "workspace:" `Text.isPrefixOf` v
        || "file:" `Text.isPrefixOf` v
        || "link:" `Text.isPrefixOf` v
        || "root:" `Text.isPrefixOf` v
        || "module:" `Text.isPrefixOf` v
        || "https://" `Text.isPrefixOf` v
        || "http://" `Text.isPrefixOf` v
        || "./" `Text.isPrefixOf` v
        || "../" `Text.isPrefixOf` v

    mkVertex :: DepType -> Text -> Text -> BunDepVertex
    mkVertex depType name version =
      BunDepVertex
        { bunDepType = depType
        , bunDepName = name
        , bunDepVersion = if Text.null version then Nothing else Just (CEq version)
        }
