{-# LANGUAGE RecordWildCards #-}

module Strategy.Maven.PluginStrategy (
  analyze',
  buildGraph,
) where

import Control.Algebra (Has, run)
import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Effect.Lift (Lift)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import DepTypes (
  DepEnvironment (..),
  DepType (MavenType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Exec (Exec)
import Effect.Grapher (Grapher, edge, evalGrapher)
import Effect.ReadFS (ReadFS)
import Graphing (Graphing)
import Path (Abs, Dir, Path)
import Strategy.Maven.Plugin (
  Artifact (..),
  Edge (..),
  PluginOutput (..),
  execPlugin,
  installPlugin,
  parsePluginOutput,
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
analyze' dir = do
  graph <- withUnpackedPlugin $ \filepath -> do
    context "Installing plugin" $ installPlugin dir filepath
    context "Running plugin" $ execPlugin dir
    pluginOutput <- parsePluginOutput dir
    context "Building dependency graph" $ pure (buildGraph pluginOutput)
  pure (graph, Complete)

buildGraph :: PluginOutput -> Graphing Dependency
buildGraph PluginOutput{..} = run $
  evalGrapher $ do
    let byNumeric :: Map Int Artifact
        byNumeric = indexBy artifactNumericId outArtifacts

    let depsByNumeric :: Map Int Dependency
        depsByNumeric = M.map toDependency byNumeric

    traverse_ (visitEdge depsByNumeric) outEdges
  where
    toDependency :: Artifact -> Dependency
    toDependency Artifact{..} =
      Dependency
        { dependencyType = MavenType
        , dependencyName = artifactGroupId <> ":" <> artifactArtifactId
        , dependencyVersion = Just (CEq artifactVersion)
        , dependencyLocations = []
        , dependencyEnvironments = [EnvTesting | "test" `elem` artifactScopes]
        , dependencyTags =
            M.fromList $
              ("scopes", artifactScopes) :
                [("optional", ["true"]) | artifactOptional]
        }

    visitEdge :: Has (Grapher Dependency) sig m => Map Int Dependency -> Edge -> m ()
    visitEdge refsByNumeric Edge{..} = do
      let refs = do
            parentRef <- M.lookup edgeFrom refsByNumeric
            childRef <- M.lookup edgeTo refsByNumeric
            Just (parentRef, childRef)

      traverse_ (uncurry edge) refs

    indexBy :: Ord k => (v -> k) -> [v] -> Map k v
    indexBy f = M.fromList . map (\v -> (f v, v))
