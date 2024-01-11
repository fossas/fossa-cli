{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Strategy.Gradle.ResolutionApi (
  ResolvedProject (..),
  ResolvedDependency (..),
  ResolvedComponent (..),
  ResolvedConfiguration (..),
  buildGraph,
  parseResolutionApiJsonDeps,
) where

import Control.Algebra (Has)
import Data.Aeson (
  FromJSON (parseJSON),
  Value (String),
  decodeStrict,
  withObject,
  (.:),
 )
import Data.Aeson.Types (Parser, unexpected)
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.String.Conversion (encodeUtf8)
import Data.Text (Text)
import DepTypes (
  DepEnvironment (EnvOther),
  DepType (MavenType, SubprojectType),
  Dependency (..),
  VerConstraint (CEq),
  insertEnvironment,
 )
import Effect.Grapher (LabeledGrapher, deep, direct, edge, label, run, withLabeling)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Strategy.Gradle.Common (
  ConfigName (unConfigName),
  GradleLabel (Env),
  ProjectName (..),
  configNameToLabel,
  getLinesWithPrefix,
  packagePathsWithJson,
 )

data ResolvedProject = ResolvedProject
  { resolvedProjectName :: ProjectName
  , resolvedProjectConfigurations :: [ResolvedConfiguration]
  }
  deriving (Show, Eq, Ord, Generic, FromJSON)

data ResolvedConfiguration = ResolvedConfiguration
  { resolvedConfigurationName :: ConfigName
  , resolvedConfigurationDirectComponents :: [ResolvedDependency]
  , resolvedConfigurationDependencies :: [ResolvedComponent]
  }
  deriving (Show, Eq, Ord, Generic, FromJSON)

data ResolvedComponent = ResolvedComponent
  { resolvedComponentNode :: ResolvedDependency
  , resolvedComponentOutgoing :: [ResolvedDependency]
  }
  deriving (Show, Eq, Ord, Generic, FromJSON)

data ResolvedDependency
  = ProjectDependency Text
  | PackageDependency Text Text
  deriving (Show, Eq, Ord)

instance FromJSON ResolvedDependency where
  parseJSON = withObject "ResolvedDependency" $ \obj -> do
    ty <- obj .: "type" :: Parser Text
    case ty of
      "project" -> ProjectDependency <$> obj .: "name"
      "package" ->
        PackageDependency
          <$> obj .: "name"
          <*> obj .: "version"
      _ -> unexpected (String ty)

parseResolutionApiJsonDeps :: Text -> [ResolvedProject]
parseResolutionApiJsonDeps text = packagePathsWithDecoded
  where
    jsonDepsLines :: [Text]
    jsonDepsLines = getLinesWithPrefix text "RESOLUTIONAPI_JSONDEPS_"

    packagePathsWithDecoded :: [ResolvedProject]
    packagePathsWithDecoded = mapMaybe (\(_, outJson) -> decodeStrict $ encodeUtf8 outJson) (packagePathsWithJson jsonDepsLines)

-- | Builds graph using gradle's resolution api.
-- Reference: https://docs.gradle.org/current/javadoc/org/gradle/api/artifacts/result/ResolutionResult.html
buildGraph :: [ResolvedProject] -> Set.Set ConfigName -> Graphing Dependency
buildGraph projects onlyConfigs = run . withLabeling toDependency $ mapM_ addConfig applicableConfigurations
  where
    applicableConfigurations :: [ResolvedConfiguration]
    applicableConfigurations = filter (isConfigIncluded . resolvedConfigurationName) configurations

    configurations :: [ResolvedConfiguration]
    configurations = concatMap resolvedProjectConfigurations projects

    isConfigIncluded :: ConfigName -> Bool
    isConfigIncluded c = (Set.null onlyConfigs) || Set.member c onlyConfigs

    addConfig :: Has (LabeledGrapher ResolvedDependency GradleLabel) sig m => ResolvedConfiguration -> m ()
    addConfig resolvedConfig = do
      let configLabel = toGradleLabel $ resolvedConfigurationName resolvedConfig

      for_ (resolvedConfigurationDirectComponents resolvedConfig) $ \directDep -> do
        label directDep configLabel
        direct directDep

      for_ (resolvedConfigurationDependencies resolvedConfig) $ \rcDepAdjacency -> do
        let parentDep = resolvedComponentNode rcDepAdjacency
        label parentDep configLabel
        deep parentDep

        for_ (resolvedComponentOutgoing rcDepAdjacency) $ \childDep -> do
          deep childDep
          label childDep configLabel
          edge parentDep childDep

    -- Infers environment label based on the name of configuration.
    -- Ref: https://docs.gradle.org/current/userguide/java_library_plugin.html#sec:java_library_configurations_graph
    toGradleLabel :: ConfigName -> GradleLabel
    toGradleLabel conf =
      if Set.null onlyConfigs
        then configNameToLabel (unConfigName conf) -- We have no specified configs, so we have to guess the correct Env.
        else Env $ EnvOther (unConfigName conf) -- We only have specified configs, so we mark them all as Other.

    toDependency :: ResolvedDependency -> Set.Set GradleLabel -> Dependency
    toDependency dep = foldr applyLabel $ fromResolvedDep dep

    applyLabel :: GradleLabel -> Dependency -> Dependency
    applyLabel lbl dep = case lbl of
      Env env -> insertEnvironment env dep

    fromResolvedDep :: ResolvedDependency -> Dependency
    fromResolvedDep (ProjectDependency name) =
      Dependency
        { dependencyType = SubprojectType
        , dependencyName = name
        , dependencyVersion = Nothing
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }
    fromResolvedDep (PackageDependency name version) =
      Dependency
        { dependencyType = MavenType
        , dependencyName = name
        , dependencyVersion = Just $ CEq version
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }
