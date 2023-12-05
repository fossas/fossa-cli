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
  warnOnErr,
  (<||>),
 )
import Control.Effect.Lift (Lift)
import Control.Monad (when)
import Data.Foldable (Foldable (foldl'), traverse_)
import Data.List (nub)
import Data.Map.Strict (Map, delete, insert, member, (!))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Debug.Trace
import Debug.Trace (traceM, traceShowM)
import DepTypes (
  DepEnvironment (..),
  DepType (MavenType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Exec (CandidateCommandEffs, exec)
import Effect.Grapher (Grapher, edge, evalGrapher)
import Effect.Grapher qualified as Grapher
import Effect.Logger (Logger, Pretty (pretty), logDebug, runLogger)
import Effect.ReadFS (ReadFS)
import Graphing (Graphing, shrinkRoots)
import Path (Abs, Dir, Path)
import Strategy.Maven.Common (MavenDependency (..))
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
import Text.Pretty.Simple (pPrint, pShow, pShowNoColor)
import Text.Show.Pretty qualified as Pr
import Types (GraphBreadth (..))

analyze' ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Logger sig m
  ) =>
  Path Abs Dir ->
  m (Graphing MavenDependency, GraphBreadth)
analyze' dir = analyze dir depGraphPlugin

analyzeLegacy' ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Logger sig m
  ) =>
  Path Abs Dir ->
  m (Graphing MavenDependency, GraphBreadth)
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
buildGraph :: ReactorOutput -> PluginOutput -> Graphing MavenDependency
buildGraph reactorOutput PluginOutput{..} =
  -- The root deps in the maven depgraph text graph output are either the
  -- toplevel package or submodules in a multi-module project. We don't want to
  -- consider those because they're the users' packages, so promote them to
  -- direct when building the graph using `shrinkRoots`.

  run . evalGrapher $ do
    -- shrinkRoots . run . evalGrapher $ do
    let byNumeric :: Map Int Artifact
        byNumeric = indexBy artifactNumericId outArtifacts
    traceM ("This is by numeric: " ++ show (byNumeric))

    traceM ("The original output Artifacts: =========")
    traceShowM (outArtifacts)

    traceM ("The original output Artifact edges: =========")
    traceShowM (outEdges)

    traceM ("The reactor out Artifacts: =========")
    traceShowM (reactorArtifacts $ reactorOutput)

    let filteredNumsByNumeric = Map.filterWithKey (\k _ -> k `Set.notMember` filterSubmoduleSet) byNumeric
    traceM ("The filtered by numeric")
    traceShowM filteredNumsByNumeric

    -- depsByNumeric <- traverse toDependency filteredNumsByNumeric
    depsByNumeric <- traverse toDependency byNumeric
    traceM ("This is deps by numeric ***********")
    traceShowM depsByNumeric

    let (updatedV, updatedE) = removeVertex 7 depsByNumeric outEdges
    -- traceM ("This is updated Deps and ############" ++ show (updatedV))
    -- -- traceM x

    -- traceM ("This is updated edges ############" ++ show (updatedE))

    let transativeDepExec = findTransitiveDependencies 7 outEdges
    let transativeDepLib = findTransitiveDependencies 6 outEdges

    traceM ("This si the transative edges of Exec ######### " ++ show (transativeDepExec))
    traceM ("This si the transative edges of Lib ######### " ++ show (transativeDepLib))

    traverse_ (visitEdge depsByNumeric) outEdges
  where
    filterArtifact :: [Artifact] -> [Artifact]
    filterArtifact = filter (\x -> artifactArtifactId x `Set.notMember` Set.fromList ["exec"])

    knownSubmodules :: Set.Set Text
    knownSubmodules = Set.fromList . map reactorArtifactName . reactorArtifacts $ reactorOutput

    filterSubmoduleSet :: Set.Set Int
    filterSubmoduleSet = Set.fromList [7]

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
          mavenDep = MavenDependency dep dependencyScopes

      when
        (artifactIsDirect || artifactArtifactId `Set.member` knownSubmodules)
        (Grapher.direct mavenDep)
      pure mavenDep

    visitEdge :: Has (Grapher MavenDependency) sig m => Map Int MavenDependency -> Edge -> m ()
    visitEdge refsByNumeric Edge{..} = do
      traceM ("WITH WHEN STATEMENT +++++++++ ")
      let refs = do
            parentRef <- Map.lookup edgeFrom refsByNumeric
            childRef <- Map.lookup edgeTo refsByNumeric
            Just (parentRef, childRef)
      traverse_ (uncurry edge) refs

    -- when
    --   (edgeFrom `Set.notMember` filterSubmoduleSet && edgeTo `Set.notMember` filterSubmoduleSet)
    --   $ traverse_ (uncurry edge) refs

    indexBy :: Ord k => (v -> k) -> [v] -> Map k v
    indexBy f = Map.fromList . map (\v -> (f v, v))

    removeVertex :: Int -> Map Int Dependency -> [Edge] -> (Map Int Dependency, [Edge])
    removeVertex targetVertex vertices edges =
      let transitiveDependencies = findTransitiveDependencies targetVertex edges
          allDependencies = targetVertex : transitiveDependencies
          updatedVertices = foldl' (\acc v -> Map.delete v acc) vertices allDependencies
          updatedEdges = filter (\(Edge from to) -> from `notElem` allDependencies && to `notElem` allDependencies) edges
          sharedDependencies = nub $ concatMap (\dep -> findDirectDependencies dep edges) allDependencies
          updatedEdgesWithShared = updatedEdges ++ filter (\(Edge from to) -> from `elem` sharedDependencies && to `elem` sharedDependencies) edges
       in (updatedVertices, updatedEdgesWithShared)

    findTransitiveDependencies :: Int -> [Edge] -> [Int]
    findTransitiveDependencies vertexId edges =
      let directDependencies = [to | Edge from to <- edges, from == vertexId && to /= 6]
          transitiveDependencies = concatMap (\dep -> if dep /= 6 then findTransitiveDependencies dep edges else []) directDependencies
       in directDependencies ++ transitiveDependencies

    findDirectDependencies :: Int -> [Edge] -> [Int]
    findDirectDependencies vertexId edges =
      [to | Edge from to <- edges, from == vertexId]
