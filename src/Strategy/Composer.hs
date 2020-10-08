{-# LANGUAGE TypeApplications #-}

module Strategy.Composer
  ( discover,
    buildGraph,
    ComposerLock (..),
    CompDep (..),
    Source (..),
  )
where

import Control.Effect.Diagnostics hiding (fromMaybe)
import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Data.Aeson.Types
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import DepTypes
import Discovery.Walk
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import Path
import Types

discover :: MonadIO m => Path Abs Dir -> m [DiscoveredProject]
discover dir = map mkProject <$> findProjects dir

findProjects :: MonadIO m => Path Abs Dir -> m [ComposerProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "composer.lock" files of
    Nothing -> pure ([], WalkContinue)
    Just lock -> do
      let project =
            ComposerProject
              { composerDir = dir,
                composerLock = lock
              }

      pure ([project], WalkContinue)

mkProject :: ComposerProject -> DiscoveredProject
mkProject project =
  DiscoveredProject
    { projectType = "composer",
      projectBuildTargets = mempty,
      projectDependencyGraph = const . runReadFSIO $ getDeps project,
      projectPath = composerDir project,
      projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => ComposerProject -> m (Graphing Dependency)
getDeps project = buildGraph <$> readContentsJson @ComposerLock (composerLock project)

data ComposerProject = ComposerProject
  { composerDir :: Path Abs Dir
  , composerLock :: Path Abs File
  } deriving (Eq, Ord, Show)

data ComposerLock = ComposerLock
  { lockPackages :: [CompDep],
    lockPackagesDev :: [CompDep]
  }
  deriving (Eq, Ord, Show)

data CompDep = CompDep
  { depName :: Text,
    depVersion :: Text,
    depSource :: Source,
    -- | name to version spec
    depRequire :: Maybe (Map Text Text),
    depRequireDev :: Maybe (Map Text Text)
  }
  deriving (Eq, Ord, Show)

data Source = Source
  { sourceType :: Text,
    sourceUrl :: Text,
    sourceReference :: Text
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
      <*> obj .: "source"
      <*> obj .:? "require"
      <*> obj .:? "require-dev"

instance FromJSON Source where
  parseJSON = withObject "Source" $ \obj ->
    Source <$> obj .: "type"
      <*> obj .: "url"
      <*> obj .: "reference"

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
      _ <- M.traverseWithKey (addEdge pkg) (fromMaybe M.empty $ depRequire dep)
      label pkg (DepVersion $ depVersion dep)
      label pkg (CompEnv env)
      direct pkg

    addEdge :: Has CompGrapher sig m => CompPkg -> Text -> Text -> m ()
    addEdge pkg name _ = edge pkg (CompPkg name)

    toDependency :: CompPkg -> Set CompLabel -> Dependency
    toDependency pkg = foldr addLabel $
      Dependency
        { dependencyType = ComposerType,
          dependencyName = pkgName pkg,
          dependencyVersion = Nothing,
          dependencyLocations = [],
          dependencyEnvironments = [],
          dependencyTags = M.empty
        }

    addLabel :: CompLabel -> Dependency -> Dependency
    addLabel (DepVersion ver) dep = dep {dependencyVersion = Just (CEq ver)}
    addLabel (CompEnv env) dep = dep {dependencyEnvironments = env : dependencyEnvironments dep}
