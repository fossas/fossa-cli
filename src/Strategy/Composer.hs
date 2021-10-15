module Strategy.Composer (
  discover,
  buildGraph,
  ComposerLock (..),
  CompDep (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Effect.Diagnostics hiding (fromMaybe)
import Data.Aeson.Types
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import DepTypes
import Discovery.Walk
import Effect.Grapher
import Effect.ReadFS
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject ComposerProject]
discover dir = context "Composer" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [ComposerProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "composer.lock" files of
    Nothing -> pure ([], WalkContinue)
    Just lock -> do
      let project =
            ComposerProject
              { composerDir = dir
              , composerLock = lock
              }

      pure ([project], WalkContinue)

mkProject :: ComposerProject -> DiscoveredProject ComposerProject
mkProject project =
  DiscoveredProject
    { projectType = "composer"
    , projectBuildTargets = mempty
    , projectPath = composerDir project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => ComposerProject -> m DependencyResults
getDeps project = context "Composer" $ do
  lock <- readContentsJson @ComposerLock (composerLock project)
  graph <- context "Building dependency graph" $ pure (buildGraph lock)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [composerLock project]
      }

data ComposerProject = ComposerProject
  { composerDir :: Path Abs Dir
  , composerLock :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ComposerProject

instance AnalyzeProject ComposerProject where
  analyzeProject _ = getDeps

data ComposerLock = ComposerLock
  { lockPackages :: [CompDep]
  , lockPackagesDev :: [CompDep]
  }
  deriving (Eq, Ord, Show)

data CompDep = CompDep
  { depName :: Text
  , depVersion :: Text
  , -- | name to version spec
    depRequire :: Maybe (Map Text Text)
  , depRequireDev :: Maybe (Map Text Text)
  }
  deriving (Eq, Ord, Show)

instance FromJSON ComposerLock where
  parseJSON = withObject "ComposerLock" $ \obj ->
    ComposerLock <$> obj .: "packages"
      <*> obj .: "packages-dev"

instance FromJSON CompDep where
  parseJSON = withObject "CompDep" $ \obj ->
    CompDep <$> obj .: "name"
      <*> obj .: "version"
      <*> obj .:? "require"
      <*> obj .:? "require-dev"

newtype CompPkg = CompPkg {pkgName :: Text}
  deriving (Eq, Ord, Show)

type CompGrapher = LabeledGrapher CompPkg CompLabel

data CompLabel
  = DepVersion Text
  | CompEnv DepEnvironment
  deriving (Eq, Ord, Show)

buildGraph :: ComposerLock -> Graphing Dependency
buildGraph lock = run . withLabeling toDependency $ do
  traverse_ (addDeps EnvProduction) $ lockPackages lock
  traverse_ (addDeps EnvDevelopment) $ lockPackagesDev lock
  where
    addDeps :: Has CompGrapher sig m => DepEnvironment -> CompDep -> m ()
    addDeps env dep = do
      let pkg = CompPkg (depName dep)
      _ <- Map.traverseWithKey (addEdge pkg) (fromMaybe Map.empty $ depRequire dep)
      label pkg (DepVersion $ depVersion dep)
      label pkg (CompEnv env)
      direct pkg

    addEdge :: Has CompGrapher sig m => CompPkg -> Text -> Text -> m ()
    addEdge pkg name _ = edge pkg (CompPkg name)

    toDependency :: CompPkg -> Set CompLabel -> Dependency
    toDependency pkg =
      foldr addLabel $
        Dependency
          { dependencyType = ComposerType
          , dependencyName = pkgName pkg
          , dependencyVersion = Nothing
          , dependencyLocations = []
          , dependencyEnvironments = mempty
          , dependencyTags = Map.empty
          }

    addLabel :: CompLabel -> Dependency -> Dependency
    addLabel (DepVersion ver) dep = dep{dependencyVersion = Just (CEq ver)}
    addLabel (CompEnv env) dep = insertEnvironment env dep
