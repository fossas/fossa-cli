{-# LANGUAGE OverloadedRecordDot #-}

module Strategy.Node.Bun.BunLock (
  analyze,
  buildGraph,

  -- * for testing
  BunLockfile (..),
  BunWorkspace (..),
  BunPackage (..),
  BunPackageDeps (..),
  parseResolution,
) where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Monad (unless)
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
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import DepTypes (
  DepEnvironment (..),
  DepType (NodeJSType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Grapher (Grapher, deep, direct, edge, evalGrapher, run)
import Effect.ReadFS (ReadFS, readContentsJsonc)
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, File, Path)

-- | Bun lockfile (bun.lock) in JSONC format.
--
-- See @docs/references/strategies/languages/nodejs/bun.md@ for the full
-- lockfile format documentation.
data BunLockfile = BunLockfile
  { lockfileVersion :: Int
  , workspaces :: Map Text BunWorkspace
  , packages :: Map Text BunPackage
  }
  deriving (Show, Eq)

data BunWorkspace = BunWorkspace
  { wsName :: Text
  , wsDependencies :: Map Text Text
  , wsDevDependencies :: Map Text Text
  , wsOptionalDependencies :: Map Text Text
  }
  deriving (Show, Eq, Ord)

-- | Resolved dependencies extracted from a package's info object.
data BunPackageDeps = BunPackageDeps
  { pkgDepsDependencies :: Map Text Text
  , pkgDepsOptionalDependencies :: Map Text Text
  , pkgDepsPeerDependencies :: Map Text Text
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
      <$> obj .: "lockfileVersion"
      <*> obj .:? "workspaces" .!= mempty
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
--   2. For each workspace, mark its declared dependencies as direct.
--   3. For each non-workspace package, add it as a deep dependency
--      and create edges to its transitive dependencies.
--   4. Filter out workspace packages from the final graph.
buildGraph :: BunLockfile -> Graphing Dependency
buildGraph lockfile = withoutWorkspacePackages . run . evalGrapher $ do
  for_ allWorkspaces $ \workspace -> do
    markDirectDeps EnvProduction workspace.wsDependencies
    markDirectDeps EnvDevelopment workspace.wsDevDependencies
    markDirectDeps EnvProduction workspace.wsOptionalDependencies

  for_ (Map.toList $ packages lockfile) $ \(_, pkg) ->
    unless (isWorkspaceRef $ pkgResolution pkg) $ do
      let parentDep = toDependency pkg
      deep parentDep
      for_ (transitiveDepNames pkg) $ \childName ->
        case Map.lookup childName (packages lockfile) of
          Nothing -> pure ()
          Just childPkg
            | isWorkspaceRef (pkgResolution childPkg) -> pure ()
            | otherwise -> edge parentDep (toDependency childPkg)
  where
    allWorkspaces :: [BunWorkspace]
    allWorkspaces = Map.elems $ workspaces lockfile

    devDepNames :: Set.Set Text
    devDepNames = Set.fromList $ concatMap (Map.keys . wsDevDependencies) allWorkspaces

    markDirectDeps :: (Has (Grapher Dependency) sig m) => DepEnvironment -> Map Text Text -> m ()
    markDirectDeps env deps =
      for_ (Map.keys deps) $ \depName ->
        case Map.lookup depName (packages lockfile) of
          Nothing -> pure ()
          Just pkg
            | isWorkspaceRef (pkgResolution pkg) -> pure ()
            | otherwise -> direct $ toDependencyWithEnv env pkg

    transitiveDepNames :: BunPackage -> [Text]
    transitiveDepNames pkg =
      Map.keys (pkgDepsDependencies $ pkgDeps pkg)
        <> Map.keys (pkgDepsOptionalDependencies $ pkgDeps pkg)
        <> Map.keys (pkgDepsPeerDependencies $ pkgDeps pkg)

    -- \| Convert a package to a Dependency, inferring environment from workspace declarations.
    -- Environment is based on whether the package name appears in any workspace's
    -- devDependencies, not on how the package is reached in the dependency graph.
    -- This means transitive deps of dev deps get EnvProduction (since they aren't
    -- themselves declared in devDependencies). This matches npm v3 and pnpm behavior.
    toDependency :: BunPackage -> Dependency
    toDependency pkg =
      let (name, version) = parseResolution (pkgResolution pkg)
          env = if Set.member name devDepNames then EnvDevelopment else EnvProduction
       in mkDep name version env

    toDependencyWithEnv :: DepEnvironment -> BunPackage -> Dependency
    toDependencyWithEnv env pkg =
      let (name, version) = parseResolution (pkgResolution pkg)
       in mkDep name version env

    mkDep :: Text -> Text -> DepEnvironment -> Dependency
    mkDep name version env =
      Dependency
        { dependencyType = NodeJSType
        , dependencyName = name
        , dependencyVersion = if Text.null version then Nothing else Just (CEq version)
        , dependencyLocations = mempty
        , dependencyEnvironments = Set.singleton env
        , dependencyTags = mempty
        }

    -- \| Check if a resolution string refers to a workspace package.
    isWorkspaceRef :: Text -> Bool
    isWorkspaceRef = Text.isInfixOf "workspace:"

    -- \| Collect workspace package names and filter them from the graph.
    withoutWorkspacePackages :: Graphing Dependency -> Graphing Dependency
    withoutWorkspacePackages = Graphing.shrink (\dep -> not $ Set.member (dependencyName dep) wsPackageNames)

    wsPackageNames :: Set.Set Text
    wsPackageNames =
      Set.fromList
        . mapMaybe wsPackageName
        $ Map.elems (packages lockfile)

    wsPackageName :: BunPackage -> Maybe Text
    wsPackageName pkg
      | isWorkspaceRef (pkgResolution pkg) = Just . fst $ parseResolution (pkgResolution pkg)
      | otherwise = Nothing
