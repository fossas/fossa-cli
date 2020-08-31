{-# LANGUAGE TypeApplications #-}

module Strategy.Composer
  ( discover,
    analyze,
    buildGraph,
    ComposerLock (..),
    CompDep (..),
    Source (..),
  )
where

import Control.Effect.Diagnostics
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Data.Aeson.Types
import Data.Foldable (find, traverse_)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import DepTypes
import Discovery.Walk
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import Path
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> fileName f == "composer.lock") files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "php-composerlock" PHPGroup $ analyze file

  pure WalkContinue

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

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m ProjectClosureBody
analyze file = mkProjectClosure file <$> readContentsJson @ComposerLock file

mkProjectClosure :: Path Abs File -> ComposerLock -> ProjectClosureBody
mkProjectClosure file lock =
  ProjectClosureBody
    { bodyModuleDir = parent file,
      bodyDependencies = dependencies,
      bodyLicenses = []
    }
  where
    dependencies =
      ProjectDependencies
        { dependenciesGraph = buildGraph lock,
          dependenciesOptimal = Optimal,
          dependenciesComplete = Complete
        }

newtype CompPkg = CompPkg {pkgName :: Text}
  deriving (Eq, Ord, Show)

type CompGrapher = LabeledGrapher CompPkg CompLabel

data CompLabel
  = DepVersion Text
  | CompEnv DepEnvironment
  deriving (Eq, Ord, Show)

buildGraph :: ComposerLock -> Graphing Dependency
buildGraph composerLock = run . withLabeling toDependency $ do
  traverse_ (addDeps EnvProduction) $ lockPackages composerLock
  traverse_ (addDeps EnvDevelopment) $ lockPackagesDev composerLock
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