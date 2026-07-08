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
  (<||>),
 )
import Control.Effect.Lift (Lift)
import Control.Effect.Path (withSystemTempDir)
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
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
import Path (Abs, Dir, Path)
import Strategy.Maven.Common (MavenDependency (..))
import Strategy.Maven.Plugin (
  Artifact (..),
  DepGraphPlugin,
  Edge (..),
  PluginOutput (..),
  ReactorOutput (ReactorOutput),
  augmentWithDuplicateEdges,
  depGraphPlugin,
  depGraphPluginLegacy,
  execPluginAggregate,
  execPluginReactor,
  execPluginVerboseGraph,
  installPlugin,
  parsePluginOutput,
  parseReactorOutput,
  parseVerboseGraphs,
  reactorArtifactName,
  reactorArtifacts,
  withUnpackedPlugin,
 )
import Types (GraphBreadth (..))

analyze' ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  m (Graphing MavenDependency, GraphBreadth)
analyze' dir = analyze dir depGraphPlugin

analyzeLegacy' ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  m (Graphing MavenDependency, GraphBreadth)
analyzeLegacy' dir = analyze dir depGraphPluginLegacy

runReactor ::
  ( CandidateCommandEffs sig m
  , Has ReadFS sig m
  , Has (Lift IO) sig m
  ) =>
  Path Abs Dir ->
  DepGraphPlugin ->
  m ReactorOutput
runReactor dir plugin =
  context "Running plugin to get submodule names" $
    withSystemTempDir "fossa-temp" $ \tempdir -> do
      warnOnErr MayIncludeSubmodule (execPluginReactor dir tempdir plugin >> parseReactorOutput tempdir)
        <||> pure (ReactorOutput [])

analyze ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  DepGraphPlugin ->
  m (Graphing MavenDependency, GraphBreadth)
analyze dir plugin = do
  graph <- withUnpackedPlugin plugin $ \filepath -> do
    context "Installing plugin" $ errCtx MvnPluginInstallFailed $ installPlugin dir filepath plugin
    reactorOutput <- runReactor dir plugin
    context "Running plugin to get dependency graph" $
      errCtx MvnPluginExecFailed $
        execPluginAggregate dir plugin
    pluginOutput <- parsePluginOutput dir
    pluginOutput' <- recoverDuplicateEdges dir plugin pluginOutput
    context "Building dependency graph" $ pure (buildGraph reactorOutput pluginOutput')
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
  Path Abs Dir ->
  DepGraphPlugin ->
  PluginOutput ->
  m PluginOutput
recoverDuplicateEdges dir plugin pluginOutput =
  context "Running plugin to recover duplicate-resolved edges" $ do
    recovered <-
      recover $
        warnOnErr DuplicateEdgesNotRecovered $ do
          execPluginVerboseGraph dir plugin
          augmentWithDuplicateEdges pluginOutput <$> parseVerboseGraphs dir
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

data MayIncludeSubmodule = MayIncludeSubmodule
instance ToDiagnostic MayIncludeSubmodule where
  renderDiagnostic MayIncludeSubmodule = do
    let header = "Failed to run reactor, submodules may be included in the output graph."
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
buildGraph :: ReactorOutput -> PluginOutput -> Graphing MavenDependency
buildGraph reactorOutput PluginOutput{..} =
  run . evalGrapher $ do
    let byNumeric :: Map Int Artifact
        byNumeric = indexBy artifactNumericId outArtifacts

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

      when
        (artifactIsDirect || artifactArtifactId `Set.member` knownSubmodules)
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
