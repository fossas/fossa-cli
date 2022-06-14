{-# LANGUAGE RecordWildCards #-}

module Strategy.Maven.PluginStrategy (
  analyze',
  analyzeLegacy',
  buildGraph,
) where

import Control.Algebra (Has, run)
import Control.Effect.Diagnostics (
  Diagnostics,
  ToDiagnostic (renderDiagnostic),
  context,
  errCtx,
  warnOnErr,
  (<||>),
 )
import Control.Effect.Lift (Lift)
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (
  DepEnvironment (..),
  DepType (MavenType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Exec (Exec)
import Effect.Grapher (Grapher, edge, evalGrapher)
import Effect.Grapher qualified as Grapher
import Effect.ReadFS (ReadFS)
import Graphing (Graphing, shrinkRoots)
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

analyze' ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m (Graphing Dependency, GraphBreadth)
analyze' dir = analyze dir depGraphPlugin

analyzeLegacy' ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m (Graphing Dependency, GraphBreadth)
analyzeLegacy' dir = analyze dir depGraphPluginLegacy

runReactor ::
  ( Has Diagnostics sig m
  , Has Exec sig m
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
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  DepGraphPlugin ->
  m (Graphing Dependency, GraphBreadth)
analyze dir plugin = do
  graph <- withUnpackedPlugin plugin $ \filepath -> do
    context "Installing plugin" $ errCtx MvnPluginInstallFailed $ installPlugin dir filepath plugin
    reactorOutput <- runReactor dir plugin
    context "Running plugin to get dependency graph" $
      errCtx MvnPluginExecFailed $ execPluginAggregate dir plugin
    pluginOutput <- parsePluginOutput dir
    context "Building dependency graph" $ pure (buildGraph reactorOutput pluginOutput)
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
buildGraph :: ReactorOutput -> PluginOutput -> Graphing Dependency
buildGraph reactorOutput PluginOutput{..} =
  -- The root deps in the maven depgraph text graph output are either the
  -- toplevel package or submodules in a multi-module project. We don't want to
  -- consider those because they're the users' packages, so promote them to
  -- direct when building the graph using `shrinkRoots`.
  shrinkRoots . run . evalGrapher $ do
    let byNumeric :: Map Int Artifact
        byNumeric = indexBy artifactNumericId outArtifacts

    depsByNumeric <- traverse toDependency byNumeric

    traverse_ (visitEdge depsByNumeric) outEdges
  where
    knownSubmodules :: Set.Set Text
    knownSubmodules = Set.fromList . map reactorArtifactName . reactorArtifacts $ reactorOutput

    toDependency :: Has (Grapher Dependency) sig m => Artifact -> m Dependency
    toDependency Artifact{..} = do
      let dep =
            Dependency
              { dependencyType = MavenType
              , dependencyName = artifactGroupId <> ":" <> artifactArtifactId
              , dependencyVersion = Just (CEq artifactVersion)
              , dependencyLocations = []
              , dependencyEnvironments = Set.fromList $ [EnvTesting | "test" `elem` artifactScopes]
              , dependencyTags =
                  Map.fromList $
                    ("scopes", artifactScopes) :
                      [("optional", ["true"]) | artifactOptional]
              }

      when
        (artifactIsDirect || artifactArtifactId `Set.member` knownSubmodules)
        (Grapher.direct dep)
      pure dep

    visitEdge :: Has (Grapher Dependency) sig m => Map Int Dependency -> Edge -> m ()
    visitEdge refsByNumeric Edge{..} = do
      let refs = do
            parentRef <- Map.lookup edgeFrom refsByNumeric
            childRef <- Map.lookup edgeTo refsByNumeric
            Just (parentRef, childRef)

      traverse_ (uncurry edge) refs

    indexBy :: Ord k => (v -> k) -> [v] -> Map k v
    indexBy f = Map.fromList . map (\v -> (f v, v))
