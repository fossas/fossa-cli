module Strategy.Gradle.ResolvedConfiguration (
  buildGraph,
  JsonDep (..),
  parseJsonDeps,
) where

import Data.Aeson (FromJSON (..), Value (..), decodeStrict, withObject, (.:))
import Data.Aeson.Types (Parser, unexpected)
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (encodeUtf8)
import Data.Text (Text)
import DepTypes (
  DepEnvironment (..),
  DepType (MavenType, SubprojectType),
  Dependency (..),
  VerConstraint (CEq),
  insertEnvironment,
 )
import Effect.Grapher (
  Has,
  LabeledGrapher,
  direct,
  edge,
  label,
  run,
  withLabeling,
 )
import Graphing (Graphing)
import Strategy.Gradle.Common (
  ConfigName (..),
  GradleLabel (..),
  PackageName (..),
  configNameToLabel,
  getLinesWithPrefix,
  packagePathsWithJson,
 )

parseJsonDeps :: Text -> Map (PackageName, ConfigName) [JsonDep]
parseJsonDeps text = Map.fromList packagePathsWithDecoded
  where
    -- Output lines from the init script are of the format:
    -- JSONDEPS_:project-path_{"configName":[{"type":"package", ...}, ...], ...}
    --
    -- See the init script's implementation for details.
    jsonDepsLines :: [Text]
    jsonDepsLines = getLinesWithPrefix text "JSONDEPS_"

    packagePathsWithDecoded :: [((PackageName, ConfigName), [JsonDep])]
    packagePathsWithDecoded = do
      (name, outJson) <- packagePathsWithJson jsonDepsLines
      let configMap = fromMaybe mempty . decodeStrict $ encodeUtf8 outJson
      (configName, deps) <- Map.toList configMap
      pure ((name, ConfigName configName), deps)

-- TODO: use LabeledGraphing to add labels for environments
buildGraph :: Map (PackageName, ConfigName) [JsonDep] -> Set ConfigName -> Graphing Dependency
buildGraph projectsAndDeps onlyConfigs = run . withLabeling toDependency $ Map.traverseWithKey addProject filteredProjectAndDeps
  where
    filteredProjectAndDeps :: Map (PackageName, ConfigName) [JsonDep]
    filteredProjectAndDeps = Map.filterWithKey (\(_, config) _ -> isConfigIncluded config) (projectsAndDeps)

    isConfigExcluded :: ConfigName -> Bool
    isConfigExcluded c = not (Set.null onlyConfigs) && Set.notMember c onlyConfigs

    isConfigIncluded :: ConfigName -> Bool
    isConfigIncluded = not . isConfigExcluded

    -- add top-level projects from the output
    addProject :: Has (LabeledGrapher JsonDep GradleLabel) sig m => (PackageName, ConfigName) -> [JsonDep] -> m ()
    addProject (projName, configName) projDeps = do
      let projAsDep = ProjectDep $ unPackageName projName
          envLabel = toGradleLabel configName
      direct projAsDep
      label projAsDep envLabel
      for_ projDeps $ \dep -> do
        edge projAsDep dep
        mkRecursiveEdges dep envLabel

    -- Infers environment label based on the name of configuration.
    -- Ref: https://docs.gradle.org/current/userguide/java_library_plugin.html#sec:java_library_configurations_graph
    toGradleLabel :: ConfigName -> GradleLabel
    toGradleLabel conf =
      if (not . Set.null $ onlyConfigs)
        then Env $ EnvOther (unConfigName conf) -- We only have specified configs, so we mark them all as Other.
        else configNameToLabel (unConfigName conf) -- We have no specified configs, so we have to guess the correct Env.
    toDependency :: JsonDep -> Set GradleLabel -> Dependency
    toDependency dep = foldr applyLabel $ jsonDepToDep dep

    applyLabel :: GradleLabel -> Dependency -> Dependency
    applyLabel lbl dep = case lbl of
      Env env -> insertEnvironment env dep

    -- build edges between deps, recursively
    mkRecursiveEdges :: Has (LabeledGrapher JsonDep GradleLabel) sig m => JsonDep -> GradleLabel -> m ()
    mkRecursiveEdges (ProjectDep x) envLabel = label (ProjectDep x) envLabel
    mkRecursiveEdges jsondep@(PackageDep _ _ deps) envLabel = do
      label jsondep envLabel
      for_ deps $ \child -> do
        edge jsondep child
        mkRecursiveEdges child envLabel

    jsonDepToDep :: JsonDep -> Dependency
    jsonDepToDep (ProjectDep name) = projectToDep name
    jsonDepToDep (PackageDep name version _) =
      Dependency
        { dependencyType = MavenType
        , dependencyName = name
        , dependencyVersion = Just (CEq version)
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

    projectToDep name =
      Dependency
        { dependencyType = SubprojectType
        , dependencyName = name
        , dependencyVersion = Nothing
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

data JsonDep
  = ProjectDep Text -- name
  | PackageDep Text Text [JsonDep] -- name version deps
  deriving (Eq, Ord, Show)

instance FromJSON JsonDep where
  parseJSON = withObject "JsonDep" $ \obj -> do
    ty <- obj .: "type" :: Parser Text
    case ty of
      "project" -> ProjectDep <$> obj .: "name"
      "package" -> PackageDep <$> obj .: "name" <*> obj .: "version" <*> obj .: "dependencies"
      _ -> unexpected (String ty)
