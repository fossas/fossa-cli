{-# LANGUAGE RecordWildCards #-}

module Strategy.Maven.PluginStrategy (
  analyze',
  analyzeLegacy',
  buildGraph,
) where

import Control.Algebra (Has, run)
import Control.Effect.Diagnostics (
  ToDiagnostic (renderDiagnostic),
  context,
  errCtx,
  recover,
  warnOnErr,
 )
import Control.Effect.Lift (Lift)
import Control.Effect.Path (withSystemTempDir)
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (
  DepEnvironment (..),
  DepType (MavenType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Exec (CandidateCommandEffs)
import Effect.Grapher (Grapher, edge, evalGrapher)
import Effect.Grapher qualified as Grapher
import Effect.ReadFS (ReadFS)
import Errata (Errata (..))
import Graphing (Graphing)
import Path (Abs, Dir, File, Path)
import Strategy.Maven.Common (MavenDependency (..))
import Strategy.Maven.Plugin (
  Artifact (..),
  DepGraphPlugin,
  Edge (..),
  PluginOutput (..),
  augmentWithDuplicateEdges,
  depGraphPlugin,
  depGraphPluginLegacy,
  execPluginAggregate,
  execPluginVerboseGraph,
  installPlugin,
  parsePluginOutput,
  parseVerboseGraphs,
  withUnpackedPlugin,
 )
import Strategy.Maven.Pom.Closure (submodulesFromCoordinate)
import Strategy.Maven.Pom.PomFile (MavenCoordinate, Pom)
import Types (GraphBreadth (..))

analyze' ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  Map MavenCoordinate (Path Abs File, Pom) ->
  Path Abs Dir ->
  m (Graphing MavenDependency, GraphBreadth)
analyze' closurePoms dir = analyze closurePoms dir depGraphPlugin

analyzeLegacy' ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  Map MavenCoordinate (Path Abs File, Pom) ->
  Path Abs Dir ->
  m (Graphing MavenDependency, GraphBreadth)
analyzeLegacy' closurePoms dir = analyze closurePoms dir depGraphPluginLegacy

analyze ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  Map MavenCoordinate (Path Abs File, Pom) ->
  Path Abs Dir ->
  DepGraphPlugin ->
  m (Graphing MavenDependency, GraphBreadth)
analyze closurePoms dir plugin = do
  graph <- withUnpackedPlugin plugin $ \filepath -> do
    context "Installing plugin" $ errCtx MvnPluginInstallFailed $ installPlugin dir filepath plugin
    -- Use a temp output dir so we always read from a known location even when POM overrides build directory
    withSystemTempDir "fossa-depgraph" $ \tempdir -> do
      context "Running plugin to get dependency graph" $
        errCtx MvnPluginExecFailed $
          execPluginAggregate dir tempdir plugin
      pluginOutput <- parsePluginOutput tempdir
      pluginOutput' <- recoverDuplicateEdges closurePoms dir plugin pluginOutput
      context "Building dependency graph" $ pure (buildGraph (submodulesFromCoordinate closurePoms) pluginOutput')
  pure (graph, Complete)

-- | Maven's dependency mediation attaches a package shared by several parents
-- to a single winning parent; the aggregate goal only reports those winning
-- edges, so a shared transitive dependency looks exclusive to one parent (see
-- 'Strategy.Maven.Plugin.mavenPluginVerboseGraphCmd'). Recover the omitted
-- edges with a second plugin run and merge them into the parsed output.
--
-- Recovery is best-effort: on any failure the aggregate output is used as-is,
-- which matches the behavior before this step existed.
recoverDuplicateEdges ::
  ( CandidateCommandEffs sig m
  , Has ReadFS sig m
  ) =>
  Map MavenCoordinate (Path Abs File, Pom) ->
  Path Abs Dir ->
  DepGraphPlugin ->
  PluginOutput ->
  m PluginOutput
recoverDuplicateEdges closurePoms dir plugin pluginOutput =
  context "Running plugin to recover duplicate-resolved edges" $ do
    recovered <-
      recover $
        warnOnErr DuplicateEdgesNotRecovered $ do
          execPluginVerboseGraph dir plugin
          augmentWithDuplicateEdges pluginOutput <$> parseVerboseGraphs closurePoms dir
    pure (fromMaybe pluginOutput recovered)

data MvnPluginInstallFailed = MvnPluginInstallFailed
instance ToDiagnostic MvnPluginInstallFailed where
  renderDiagnostic (MvnPluginInstallFailed) = do
    let header = "Failed to install maven plugin for analysis"
    Errata (Just header) [] Nothing

data MvnPluginExecFailed = MvnPluginExecFailed
instance ToDiagnostic MvnPluginExecFailed where
  renderDiagnostic (MvnPluginExecFailed) = do
    let header = "Failed to execute maven plugin for analysis"
    Errata (Just header) [] Nothing

data DuplicateEdgesNotRecovered = DuplicateEdgesNotRecovered
instance ToDiagnostic DuplicateEdgesNotRecovered where
  renderDiagnostic DuplicateEdgesNotRecovered = do
    let header = "Failed to recover duplicate-resolved dependency edges; transitive dependencies shared between multiple parents may be attributed to only one of them."
    Errata (Just header) [] Nothing

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
--
-- TODO(#maven-parentless-modules): 'knownSubmodules' is derived from the POM
-- closure graph, which builds edges only from <parent> elements. A module
-- listed in <modules> but lacking a <parent> element (legal Maven) will not
-- appear here, so it won't be promoted to direct and its verbose-graph
-- duplicate edges may be dropped. This is a rare case; fixing it requires
-- seeding <modules> edges into the closure graph in Pom/Resolver.hs.
buildGraph :: Set Text -> PluginOutput -> Graphing MavenDependency
buildGraph knownSubmodules PluginOutput{..} =
  run . evalGrapher $ do
    let byNumeric :: Map Int Artifact
        byNumeric = indexBy artifactNumericId outArtifacts

    depsByNumeric <- traverse toDependency byNumeric

    traverse_ (visitEdge depsByNumeric) outEdges
  where
    toBuildTag :: Text -> DepEnvironment
    toBuildTag = \case
      "compile" -> EnvProduction
      "test" -> EnvTesting
      other -> EnvOther other

    toDependency :: Has (Grapher MavenDependency) sig m => Artifact -> m MavenDependency
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
          mavenDep = MavenDependency dep dependencyScopes mempty

      -- closureSubmodules uses "groupId:artifactId" coordinate form, so
      -- matching must build that coordinate from the artifact; bare artifact
      -- ids are intentionally not matched.
      when
        ( artifactIsDirect
            || (artifactGroupId <> ":" <> artifactArtifactId) `Set.member` knownSubmodules
        )
        (Grapher.direct mavenDep)
      pure mavenDep

    visitEdge :: Has (Grapher MavenDependency) sig m => Map Int MavenDependency -> Edge -> m ()
    visitEdge refsByNumeric Edge{..} = do
      let refs = do
            parentRef <- Map.lookup edgeFrom refsByNumeric
            childRef <- Map.lookup edgeTo refsByNumeric
            Just (parentRef, childRef)

      traverse_ (uncurry edge) refs

    indexBy :: Ord k => (v -> k) -> [v] -> Map k v
    indexBy f = Map.fromList . map (\v -> (f v, v))
