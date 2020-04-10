module Strategy.Maven.PluginStrategy
  ( discover
  , analyze
  , buildGraph
  ) where

import Prologue

import Control.Carrier.Error.Either
import Control.Effect.Exception
import qualified Data.Map.Strict as M
import DepTypes
import Discovery.Walk
import Effect.Exec
import Effect.Grapher hiding (Edge)
import Effect.ReadFS
import Graphing (Graphing)
import Strategy.Maven.Plugin
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> fileName f == "pom.xml") files of
    Nothing -> pure WalkContinue
    Just file -> do
      runSimpleStrategy "maven-cli" MavenGroup $ analyze (parent file)
      pure WalkSkipAll

analyze ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has (Error ReadFSErr) sig m
  , Has Exec sig m
  , Has (Error ExecErr) sig m
  , MonadIO m
  )
  => Path Rel Dir -> m ProjectClosureBody
analyze dir = withUnpackedPlugin $ \filepath -> do
  installPlugin dir filepath
  execPlugin dir
  pluginOutput <- parsePluginOutput dir
  pure (mkProjectClosure dir pluginOutput)

mkProjectClosure :: Path Rel Dir -> PluginOutput -> ProjectClosureBody
mkProjectClosure dir pluginOutput = ProjectClosureBody
  { bodyModuleDir    = dir
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph pluginOutput
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = Complete
    }

buildGraph :: PluginOutput -> Graphing Dependency
buildGraph PluginOutput{..} = run $ evalGrapher $ do
  let byNumeric :: Map Int Artifact
      byNumeric = indexBy artifactNumericId outArtifacts

  let depsByNumeric :: Map Int Dependency
      depsByNumeric = M.map toDependency byNumeric

  traverse_ (visitEdge depsByNumeric) outEdges

  where

  toDependency :: Artifact -> Dependency
  toDependency Artifact{..} = Dependency
    { dependencyType = MavenType
    , dependencyName = artifactGroupId <> ":" <> artifactArtifactId
    , dependencyVersion = Just (CEq artifactVersion)
    , dependencyLocations = []
    , dependencyEnvironments = if "test" `elem` artifactScopes then [EnvTesting] else []
    , dependencyTags = M.fromList $
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
