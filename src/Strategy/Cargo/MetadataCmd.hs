module Strategy.Cargo.MetadataCmd (
  buildGraph,
  cargoGenLockfileCmd,
  cargoMetadataCmd,
  extractGitCommitHash,
  parseGitRepoUrl,
  FailedToGenLockFile (..),
  FailedToRetrieveCargoMetadata (..),
) where

import Control.Effect.Diagnostics (run)
import Control.Monad (guard)
import Data.Foldable (for_, traverse_)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (toString, toText)
import Data.Text (Text, breakOn)
import Data.Text qualified as Text
import Diag.Diagnostic (ToDiagnostic (..))
import Effect.Exec (AllowErr (Never), Command (..))
import Effect.Grapher (direct, edge, label, withLabeling)
import Errata (Errata (..))
import Graphing (Graphing, shrinkRoots)
import Network.URI (parseURI, uriAuthority, uriPath, uriRegName)
import Path (Abs, File, Path)
import Strategy.Cargo.Types (CargoMetadata (..), NodeDepKind (..), NodeDependency (..), Package (..), PackageId (..), Resolve (..), ResolveNode (..))
import Types (DepEnvironment (EnvDevelopment, EnvProduction), DepType (CargoType, UnresolvedPathType), Dependency (..), VerConstraint (CEq), insertEnvironment)

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
    -- When the server doesn't support git-backed locators, fall back to the plain crate name.
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

newtype CargoLabel = CargoDepKind DepEnvironment
  deriving (Eq, Ord, Show)

type PackageIdSourceKind = Text.Text
type PackageIdSourceProtocol = Text.Text

-- A Cargo edge's kind ("build", "dev", or null) reflects the parent's manifest
-- declaration, not the path taken to reach the parent. We classify each package
-- by which workspace-rooted paths can reach it:
--
--   * Production: reachable from a workspace member via a path of null-kind
--     edges only. These packages are linked into the release artifact.
--
--   * Development: any package reachable (via any edge) from the target of a
--     non-null-kind edge. A "build" or "dev" edge marks the start of a subtree
--     that never ships in the release binary, and every descendant of that
--     subtree inherits Development.
--
-- A package can carry both labels when it's reachable by both kinds of paths.
--
-- We do not need a separate "dev-deps of prod-deps" case: Cargo only resolves
-- dev-dependencies for workspace members, so non-workspace edges with kind
-- "dev" do not appear in 'cargo metadata' output. The only non-null kind we
-- see on a non-workspace edge is "build".
--
-- Cargo is the only strategy with per-edge kinds; others (pnpm, yarn, poetry)
-- label nodes and propagate with 'hydrateDepEnvs'. That helper walks from a
-- labeled node to every dependency it declares, regardless of edge kind, so
-- a Production label on a workspace member would flow through a "dev" or
-- "build" edge and mislabel the dev/build subtree as Production. We roll our
-- own edge-filtered reachability here rather than generalize the shared helper.
buildGraph :: Bool -> CargoMetadata -> Graphing Dependency
buildGraph emitGitBackedLocators meta = shrinkRoots $
  run . withLabeling (toDependency emitGitBackedLocators sourceMap) $ do
    traverse_ direct (metadataWorkspaceMembers meta)
    for_ nodes $ \node ->
      for_ (resolveNodeDeps node) $ \dep ->
        edge (resolveNodeId node) (nodePkg dep)
    for_ (Set.toList prodReachable) $ \pkg ->
      label pkg (CargoDepKind EnvProduction)
    for_ (Set.toList devReachable) $ \pkg ->
      label pkg (CargoDepKind EnvDevelopment)
  where
    sourceMap = buildPackageSourceMap $ metadataPackages meta
    nodes = resolvedNodes (metadataResolve meta)
    workspaceMembers = Set.fromList (metadataWorkspaceMembers meta)

    -- These predicates are not mutually exclusive: a dep declared in both
    -- [dependencies] and [dev-dependencies] on the same parent carries both
    -- a null and a non-null kind, so the edge feeds prodAdj *and* devSeeds.
    isProdEdge dep = any (isNothing . nodeDepKind) (nodeDepKinds dep)
    isDevEdge dep = any (isJust . nodeDepKind) (nodeDepKinds dep)

    -- Adjacency containing only edges whose parent declares the child as a
    -- normal dependency (at least one kind is null). Production reachability
    -- must only traverse these — a build or dev edge breaks the release chain.
    prodAdj =
      Map.fromList $
        map
          (\node -> (resolveNodeId node, map nodePkg . filter isProdEdge . resolveNodeDeps $ node))
          nodes

    -- Every edge in the metadata graph, for Development reachability.
    allAdj =
      Map.fromList $
        map (\node -> (resolveNodeId node, map nodePkg (resolveNodeDeps node))) nodes

    -- Targets of any non-null-kind edge. Each seeds a Development subtree:
    -- the target and all its transitive descendants are never linked into
    -- a release build.
    devSeeds =
      Set.fromList $
        map nodePkg $
          concatMap (filter isDevEdge . resolveNodeDeps) nodes

    prodReachable = reachable prodAdj workspaceMembers
    devReachable = reachable allAdj devSeeds

reachable :: Map.Map PackageId [PackageId] -> Set PackageId -> Set PackageId
reachable adj = go Set.empty . Set.toList
  where
    go visited [] = visited
    go visited (x : xs) =
      if Set.member x visited
        then go visited xs
        else
          let children = fromMaybe [] (Map.lookup x adj)
           in go (Set.insert x visited) (children ++ xs)
