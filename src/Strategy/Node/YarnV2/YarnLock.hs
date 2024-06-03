{-# LANGUAGE RecordWildCards #-}

module Strategy.Node.YarnV2.YarnLock (
  analyze,
  stitchLockfile,
  buildGraph,
  FlatPackages (..),
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Extra qualified as AME
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  ToDiagnostic (renderDiagnostic),
  context,
  warnLeft,
 )
import Control.Effect.Diagnostics qualified as Diag (fromMaybe)
import Data.Either.Combinators (maybeToRight)
import Data.Foldable (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (toText)
import Data.Tagged (Tagged, applyTag, unTag)
import Data.Text qualified as Text
import Data.Text.Extra (showT)
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType, NodeJSType, URLType),
  Dependency (..),
  VerConstraint (CEq),
  hydrateDepEnvs,
  insertEnvironment,
 )
import Effect.ReadFS (ReadFS, readContentsYaml)
import Graphing (Graphing)
import Graphing qualified
import Path (Abs, File, Path)
import Strategy.Node.PackageJson (
  Development,
  FlatDeps (..),
  Manifest,
  NodePackage (..),
  Production,
 )
import Strategy.Node.YarnV2.Lockfile (
  Descriptor (descriptorName, descriptorRange, descriptorScope),
  Locator,
  PackageDescription (descDependencies, descResolution),
  YarnLockfile (..),
  tryParseDescriptor,
 )
import Strategy.Node.YarnV2.Resolvers (
  Package (..),
  resolveLocatorToPackage,
 )

newtype NoPackageForDescriptor = NoPackageForDescriptor Descriptor
  deriving (Eq, Ord, Show)

instance ToDiagnostic NoPackageForDescriptor where
  renderDiagnostic (NoPackageForDescriptor d) =
    renderDiagnostic $ "Couldn't find package for descriptor: " <> toText (show d)

data YarnLockError
  = DescriptorParse NodePackage
  | NoPackagesFound [Manifest]
  deriving (Eq, Ord, Show)

instance ToDiagnostic YarnLockError where
  renderDiagnostic err =
    case err of
      (DescriptorParse nodePkg) -> renderDiagnostic $ "Failed to parse package descriptor: " <> showT nodePkg
      (NoPackagesFound manifests) ->
        renderDiagnostic $ "No resolvable deps found in package.json(s): " <> Text.intercalate "," (map toText manifests)

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> FlatDeps -> m (Graphing Dependency)
analyze file flatdeps = context "Lockfile V2 analysis" $ do
  lockfile <- context "Reading lockfile" $ readContentsYaml @YarnLockfile file
  stitched <- context "Validating lockfile" $ stitchLockfile lockfile
  packageGraph <- context "Resolving yarn locators" $ AME.gtraverse resolveLocatorToPackage stitched
  flatpkgs <- context "Resolving direct and dev from package.json" $ resolveFlatDeps lockfile flatdeps
  context "Building dependency graph" . pure $ buildGraph packageGraph flatpkgs

resolveFlatDeps :: Has Diagnostics sig m => YarnLockfile -> FlatDeps -> m FlatPackages
resolveFlatDeps lockfile FlatDeps{..} = FlatPackages <$> converter @Production directDeps <*> converter @Development devDeps
  where
    resolvePackages :: Has Diagnostics sig m => [NodePackage] -> m [Package]
    resolvePackages = fmap catMaybes . traverse (resolveSingle lockfile)

    converter ::
      forall tag sig m.
      Has Diagnostics sig m =>
      Tagged tag (Set NodePackage) ->
      m (Tagged tag (Set Package))
    converter = fmap (applyTag . Set.fromList) . resolvePackages . Set.toList . unTag

resolveSingle :: Has Diagnostics sig m => YarnLockfile -> NodePackage -> m (Maybe Package)
resolveSingle (YarnLockfile lockfileMap) nodePkg =
  do
    descriptor <-
      Diag.fromMaybe (DescriptorParse nodePkg) $
        tryParseDescriptor nodePkg
    let lookupRes = lookupPackage descriptor $ remap lockfileMap
    res <- warnLeft lookupRes
    traverse (resolveLocatorToPackage . descResolution) res

remap :: Ord k => Map [k] a -> Map k a
remap = Map.fromList . concatMap (\(ks, v) -> map (,v) ks) . Map.toList

-- | Validate and stitch together a yarn lockfile into a graph of yarn Locators
--
-- This ensures that all dependency relationships are valid
stitchLockfile :: Has Diagnostics sig m => YarnLockfile -> m (AM.AdjacencyMap Locator)
stitchLockfile (YarnLockfile lockfile) = context ("Stitching Lockfile") $
  do graph
  where
    -- remapping @Map [Descriptor] PackageDescription@ to @Map Descriptor PackageDescription@
    remapped :: Map Descriptor PackageDescription
    remapped = remap lockfile

    lookupPackage' :: Has Diagnostics sig m => Descriptor -> m (Maybe PackageDescription)
    lookupPackage' pkg = warnLeft $ lookupPackage pkg remapped

    -- look up all of a package's dependencies as locators in the lockfile
    lookupPackageDeps :: Has Diagnostics sig m => PackageDescription -> m [Locator]
    lookupPackageDeps = fmap (map descResolution . catMaybes) . traverse lookupPackage' . descDependencies

    -- build the edges (adjacency list) between a package and its dependencies
    packageToEdges :: Has Diagnostics sig m => PackageDescription -> m [(Locator, Locator)]
    packageToEdges package = map (descResolution package,) <$> lookupPackageDeps package

    -- combine the edges produced by calling packageToEdges on each package in the lockfile
    graphEdges :: Has Diagnostics sig m => m (AM.AdjacencyMap Locator)
    graphEdges = fmap (AM.edges . concat) . traverse packageToEdges . Map.elems $ lockfile

    -- not all packages will be part of an edge, so add vertices for each package
    graphVertices :: AM.AdjacencyMap Locator
    graphVertices = AM.vertices (map descResolution (Map.elems lockfile))

    -- combine edges and vertices into a final graph
    graph :: Has Diagnostics sig m => m (AM.AdjacencyMap Locator)
    graph = AM.overlay graphVertices <$> graphEdges

-- | look up a package by trying:
-- 1. the descriptor, verbatim
-- 2. the descriptor with its range prefixed by @npm:@
-- 3. any other descriptor with a matching scope/name and an @npm:@ prefix to its range
--
-- For (2), search for "defaultProtocol" in the Resolvers module or in the
-- yarnv2 devdocs for more context about why this is necessary
--
-- For (3), yarn coalesces matching semver range subsets in descriptors for
-- npm dependencies. For example, given dependencies on @package: ^1.0.0@
-- and @package: ^2.0.0@, only @package@npm:^2.0.0@ will appear as a
-- descriptor key for a package in the lockfile
lookupPackage :: Descriptor -> Map Descriptor PackageDescription -> Either NoPackageForDescriptor PackageDescription
lookupPackage desc mapping =
  maybeToRight (NoPackageForDescriptor desc) $
    Map.lookup desc mapping <|> Map.lookup (desc{descriptorRange = "npm:" <> descriptorRange desc}) mapping <|> lookupAnyNpm desc mapping
  where
    -- find any package with a descriptor with matching scope/name, and an @npm:@ prefix prefix
    lookupAnyNpm :: Descriptor -> Map Descriptor PackageDescription -> Maybe PackageDescription
    lookupAnyNpm desc' mapping' = find (\other -> identMatches desc' other && "npm:" `Text.isPrefixOf` descriptorRange other) (Map.keys mapping') >>= (`Map.lookup` mapping')

    -- whether the scope and name of the package matches in both descriptors
    identMatches :: Descriptor -> Descriptor -> Bool
    identMatches one two = descriptorScope one == descriptorScope two && descriptorName one == descriptorName two

data FlatPackages = FlatPackages
  { directSet :: Tagged Production (Set Package)
  , devSet :: Tagged Development (Set Package)
  }
  deriving (Eq, Ord, Show)

instance Semigroup FlatPackages where
  (<>) (FlatPackages direct1 dev1) (FlatPackages direct2 dev2) = FlatPackages (direct1 <> direct2) (dev1 <> dev2)

instance Monoid FlatPackages where
  mempty = FlatPackages mempty mempty

-- | Turn a graph of packages into a dependency graph
--
-- Because workspaces are top-level projects, we set their dependencies as
-- direct in the dependency graph
buildGraph :: AM.AdjacencyMap Package -> FlatPackages -> Graphing Dependency
buildGraph gr FlatPackages{..} = hydrateDepEnvs convertedGraphing
  where
    isWorkspace WorkspacePackage{} = True
    isWorkspace _ = False

    -- workspaces are the "direct" dependencies
    directPackages :: [Package]
    directPackages = filter isWorkspace (AM.vertexList gr)

    -- a Graphing containing only the direct deps
    directGraphing :: Graphing Package
    directGraphing = Graphing.fromList directPackages
    -- a Graphing containing the full Package graph, but without any deps marked as direct
    transitiveGraphing :: Graphing Package
    transitiveGraphing = Graphing.fromAdjacencyMap gr

    -- combine direct and transitive graphs; eliminate workspaces by stripping
    -- the root (the dependencies of the workspaces become direct dependencies)
    completeGraphing :: Graphing Package
    completeGraphing = Graphing.stripRoot $ directGraphing <> transitiveGraphing

    -- convert Packages in the graph to Dependencies
    convertedGraphing :: Graphing Dependency
    convertedGraphing = Graphing.induceJust . Graphing.gmap convertPackage $ completeGraphing

    convertPackage :: Package -> Maybe Dependency
    convertPackage pkg = promote pkg <$> packageToDependency pkg

    promote :: Package -> Dependency -> Dependency
    promote pkg =
      promoteSet @Development EnvDevelopment devSet pkg
        . promoteSet @Production EnvProduction directSet pkg

    promoteSet ::
      DepEnvironment ->
      Tagged tag (Set Package) ->
      Package ->
      Dependency ->
      Dependency
    promoteSet env taggedSet pkg =
      if Set.member pkg $ unTag taggedSet
        then insertEnvironment env
        else id

-- | Convert a yarn package to a fossa Dependency
--
-- Dependency types that aren't supported return Nothing
packageToDependency :: Package -> Maybe Dependency
packageToDependency WorkspacePackage{} = Nothing
packageToDependency FilePackage{} = Nothing
packageToDependency LibPackage{} = Nothing
packageToDependency LinkPackage{} = Nothing
packageToDependency PortalPackage{} = Nothing
packageToDependency ExecPackage{} = Nothing
packageToDependency PatchPackage{} = Nothing
packageToDependency (NpmPackage maybeScope name version) =
  Just
    Dependency
      { dependencyType = NodeJSType
      , dependencyName =
          case maybeScope of
            Nothing -> name
            Just scope -> "@" <> scope <> "/" <> name
      , dependencyVersion = Just (CEq version)
      , dependencyLocations = []
      , dependencyTags = Map.empty
      , dependencyEnvironments = mempty
      }
packageToDependency (GitPackage repo commit) =
  Just
    Dependency
      { dependencyType = GitType
      , dependencyName = repo
      , dependencyVersion = Just (CEq commit)
      , dependencyLocations = []
      , dependencyTags = Map.empty
      , dependencyEnvironments = mempty
      }
packageToDependency (TarPackage url) =
  Just
    Dependency
      { dependencyType = URLType
      , dependencyName = url
      , dependencyVersion = Nothing
      , dependencyLocations = []
      , dependencyTags = Map.empty
      , dependencyEnvironments = mempty
      }
