{-# LANGUAGE RecordWildCards #-}

module Strategy.Maven.PluginStrategy (
  analyze',
  analyzeLegacy',
  buildGraph,
  mavenDepToDependency,
  filterMavenDepByScope,
  MavenDep (..),
) where

import Control.Algebra (Has, run)
import Control.Effect.Diagnostics (
  ToDiagnostic (renderDiagnostic),
  context,
  errCtx,
  warnOnErr,
  (<||>),
 )
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, asks)
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Debug.Trace (traceM)
import DepTypes (
  DepEnvironment (..),
  DepType (MavenType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Discovery.Filters (FilterSet (scopes), MavenScopeFilters (excludeScope, includeScope))
import Effect.Exec (CandidateCommandEffs)
import Effect.Grapher (Grapher, edge, evalGrapher)
import Effect.Grapher qualified as Grapher
import Effect.ReadFS (ReadFS)
import Graphing (Graphing, filter, shrinkRoots)
import Path (Abs, Dir, Path)
import Strategy.Maven.Plugin (
  Artifact (..),
  DepGraphPlugin,
  Edge (..),
  PluginOutput (..),
  ReactorOutput (ReactorOutput),
  depGraphPlugin,
  depGraphPluginLegacy,
  execPluginAggregate,
  execPluginReactor,
  installPlugin,
  parsePluginOutput,
  parseReactorOutput,
  reactorArtifactName,
  reactorArtifacts,
  withUnpackedPlugin,
 )
import Types (GraphBreadth (..))

data MavenDep = MavenDep
  { dependency :: Dependency
  , dependencyScopes :: Set Text
  }
  deriving (Eq, Ord, Show)

analyze' ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has (Reader MavenScopeFilters) sig m
  ) =>
  Path Abs Dir ->
  m (Graphing MavenDep, GraphBreadth)
analyze' dir = analyze dir depGraphPlugin

analyzeLegacy' ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has (Reader MavenScopeFilters) sig m
  ) =>
  Path Abs Dir ->
  m (Graphing MavenDep, GraphBreadth)
analyzeLegacy' dir = analyze dir depGraphPluginLegacy

runReactor ::
  ( CandidateCommandEffs sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  DepGraphPlugin ->
  m ReactorOutput
runReactor dir plugin =
  context "Running plugin to get submodule names" $
    warnOnErr MayIncludeSubmodule (execPluginReactor dir plugin >> parseReactorOutput dir)
      <||> pure (ReactorOutput [])

analyze ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has (Reader MavenScopeFilters) sig m
  ) =>
  Path Abs Dir ->
  DepGraphPlugin ->
  m (Graphing MavenDep, GraphBreadth)
analyze dir plugin = do
  includeScopeFilters <- asks includeScope
  excludeScopeFilters <- asks excludeScope
  let includeScopeFilterSet = scopes includeScopeFilters
      excludeScopeFilterSet = scopes excludeScopeFilters
  graph <- withUnpackedPlugin plugin $ \filepath -> do
    context "Installing plugin" $ errCtx MvnPluginInstallFailed $ installPlugin dir filepath plugin
    reactorOutput <- runReactor dir plugin
    context "Running plugin to get dependency graph" $
      errCtx MvnPluginExecFailed $
        execPluginAggregate dir plugin
    pluginOutput <- parsePluginOutput dir
    context "Building dependency graph" $ pure (buildGraph reactorOutput pluginOutput includeScopeFilterSet excludeScopeFilterSet)
  pure (graph, Complete)

data MvnPluginInstallFailed = MvnPluginInstallFailed
instance ToDiagnostic MvnPluginInstallFailed where
  renderDiagnostic (MvnPluginInstallFailed) = "Failed to install maven plugin for analysis."

data MvnPluginExecFailed = MvnPluginExecFailed
instance ToDiagnostic MvnPluginExecFailed where
  renderDiagnostic (MvnPluginExecFailed) = "Failed to execute maven plugin for analysis."

data MayIncludeSubmodule = MayIncludeSubmodule
instance ToDiagnostic MayIncludeSubmodule where
  renderDiagnostic MayIncludeSubmodule =
    "Failed to run reactor, submodules may be included in the output graph."

-- | The graphs returned by the depgraph plugin look like this:
--
-- @
-- org1:toplevelPackage1:1.0.0:compile
-- \- org1:name2:2.0.0:compile
-- @
--
-- Multimodule projects look like this:
--
-- @
-- org1:submodule2:1.0.0:compile
-- \- org1:name3:3.0.0:compile
--    \- org1:submodule1:1.0.0:compile
--       \- org1:name2:2.0.0:compile
-- @
--
-- After building a graph from the text, we do some additional processing. In
-- both cases, we want to remove either the toplevel project name or the
-- submodule name because these are the users' own packages.
--
-- The multimodule case shows how one submodule can depend on another. In this
-- case we want to remove the reference to submodule1 in submodule2's dependency
-- tree and promote submodule1's dependency to be a root (direct) dependency.
buildGraph :: ReactorOutput -> PluginOutput -> Set Text -> Set Text -> Graphing MavenDep
buildGraph reactorOutput PluginOutput{..} scopeIncludeSet scopeExcludeSet =
  -- The root deps in the maven depgraph text graph output are either the
  -- toplevel package or submodules in a multi-module project. We don't want to
  -- consider those because they're the users' packages, so promote them to
  -- direct when building the graph using `shrinkRoots`.

  shrinkRoots . run . evalGrapher $ do
    let byNumeric :: Map Int Artifact
        -- byNumeric = indexBy artifactNumericId applicableArtificats
        byNumeric = indexBy artifactNumericId outArtifacts

    -- traceM ("byNumeric in Maven Plugin Strategy ---------- " ++ show (byNumeric))
    depsByNumeric <- traverse toDependency byNumeric

    traverse_ (visitEdge depsByNumeric) outEdges
  where
    knownSubmodules :: Set.Set Text
    knownSubmodules = Set.fromList . map reactorArtifactName . reactorArtifacts $ reactorOutput

    toBuildTag :: Text -> DepEnvironment
    toBuildTag = \case
      "compile" -> EnvProduction
      "test" -> EnvTesting
      other -> EnvOther other

    toDependency :: Has (Grapher MavenDep) sig m => Artifact -> m MavenDep
    toDependency Artifact{..} = do
      let dep =
            Dependency
              { dependencyType = MavenType
              , dependencyName = artifactGroupId <> ":" <> artifactArtifactId
              , dependencyVersion = Just (CEq artifactVersion)
              , dependencyLocations = []
              , dependencyEnvironments = Set.fromList $ toBuildTag <$> artifactScopes
              , dependencyTags =
                  Map.fromList $
                    ("scopes", artifactScopes)
                      : [("optional", ["true"]) | artifactOptional]
              }
          dependencyScopes = Set.fromList artifactScopes
          mavenDep = MavenDep dep dependencyScopes

      traceM ("**** This is the dep in toDependency: " ++ show (dep))
      traceM ("**** Artificat Scope : " ++ show (artifactScopes))
      when
        (artifactIsDirect || artifactArtifactId `Set.member` knownSubmodules)
        (Grapher.direct mavenDep)
      -- pure dep
      pure mavenDep

    visitEdge :: Has (Grapher MavenDep) sig m => Map Int MavenDep -> Edge -> m ()
    visitEdge refsByNumeric Edge{..} = do
      let refs = do
            parentRef <- Map.lookup edgeFrom refsByNumeric
            childRef <- Map.lookup edgeTo refsByNumeric
            Just (parentRef, childRef)

      traverse_ (uncurry edge) refs

    indexBy :: Ord k => (v -> k) -> [v] -> Map k v
    indexBy f = Map.fromList . map (\v -> (f v, v))

-- applicableArtificats :: [Artifact]
-- applicableArtificats = filter (isArtifactIncludedScopeFilter . artifactScopes) outArtifacts

-- isArtifactIncludedScopeFilter :: [Text] -> Bool
-- isArtifactIncludedScopeFilter = all (\x -> (Set.member x scopeIncludeSet) && (not (Set.member x scopeExcludeSet)))

-- makeMavenDep :: Dependency -> MavenDep

mavenDepToDependency :: MavenDep -> Dependency
mavenDepToDependency MavenDep{..} = dependency

filterMavenDepByScope :: Set Text -> Set Text -> Graphing MavenDep -> Graphing MavenDep
filterMavenDepByScope scopeIncludeSet scopeExcludeSet = Graphing.filter includeDep
  where
    includeDep :: MavenDep -> Bool
    includeDep MavenDep{..} = case (Set.null scopeIncludeSet, Set.null scopeExcludeSet) of
      (True, True) -> True
      (False, True) -> dependencyScopes `Set.isSubsetOf` scopeIncludeSet
      (True, False) -> dependencyScopes `Set.disjoint` scopeExcludeSet
      (False, False) -> (dependencyScopes `Set.isSubsetOf` scopeIncludeSet) && (dependencyScopes `Set.disjoint` scopeExcludeSet)
