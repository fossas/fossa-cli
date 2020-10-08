{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Strategy.Haskell.Stack
  ( discover,
    -- * Testing
    buildGraph,
    PackageName (..),
    StackDep (..),
    StackLocation (..),
  )
where

import Control.Effect.Diagnostics
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.Types
import Data.Foldable (for_)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Discovery.Walk
import Effect.Exec
import Effect.Grapher
import qualified Graphing as G
import Path
import Types
import Prelude
import Control.Monad (when)

newtype PackageName = PackageName {unPackageName :: Text} deriving (FromJSON, Eq, Ord, Show)

data StackDep = StackDep
  { stackName :: PackageName,
    stackVersion :: Text,
    stackDepNames :: [PackageName],
    stackLocation :: StackLocation
  }
  deriving (Eq, Ord, Show)

data StackLocation
  = Local
  | Remote
  | BuiltIn
  deriving (Eq, Ord, Show)

instance FromJSON StackDep where
  parseJSON = withObject "StackDep" $ \obj ->
    StackDep <$> obj .: "name"
      <*> obj .: "version"
      <*> obj .:? "dependencies" .!= []
      <*> obj .:? "location" .!= BuiltIn

instance FromJSON StackLocation where
  parseJSON = withObject "StackLocation" $ \obj -> obj .: "type" >>= parseLocationType

parseLocationType :: MonadFail m => Text -> m StackLocation
parseLocationType txt
  | txt == "hackage" = pure Remote
  | txt `elem` ["project package", "archive"] = pure Local
  | otherwise = fail $ "Bad location type: " ++ T.unpack txt

discover :: MonadIO m => Path Abs Dir -> m [DiscoveredProject]
discover dir = map mkProject <$> findProjects dir

findProjects :: MonadIO m => Path Abs Dir -> m [StackProject]
findProjects = walk' $ \dir _ files -> do
  let project =
        StackProject
          { stackDir = dir
          }

  case findFileNamed "stack.yaml" files of
    Nothing -> pure ([], WalkContinue)
    Just _ -> pure ([project], WalkSkipAll)

mkProject :: StackProject -> DiscoveredProject
mkProject project =
  DiscoveredProject
    { projectType = "stack",
      projectBuildTargets = mempty,
      projectDependencyGraph = const . runExecIO $ getDeps project,
      projectPath = stackDir project,
      projectLicenses = pure []
    }

getDeps :: (Has Exec sig m, Has Diagnostics sig m) => StackProject -> m (G.Graphing Dependency)
getDeps = analyze . stackDir

data StackProject = StackProject
  { stackDir :: Path Abs Dir
  } deriving (Eq, Ord, Show)

stackJSONDepsCmd :: Command
stackJSONDepsCmd =
  Command
    { cmdName = "stack",
      cmdArgs = ["ls", "dependencies", "json"],
      cmdAllowErr = Never
    }

doGraph :: Has (MappedGrapher PackageName StackDep) sig m => StackDep -> m ()
doGraph dep = do
  let name = stackName dep
  mapping name dep
  for_ (stackDepNames dep) $ \child -> do
    edge name child
    when (stackLocation dep == Local) (direct child)

ignorePackageName :: PackageName -> a -> a
ignorePackageName = flip const

shouldInclude :: StackDep -> Bool
shouldInclude dep = Remote == stackLocation dep

toDependency :: StackDep -> Dependency
toDependency dep =
  Dependency
    { dependencyType = HackageType,
      dependencyName = unPackageName $ stackName dep,
      dependencyVersion = Just $ CEq $ stackVersion dep,
      dependencyLocations = [],
      dependencyEnvironments = [],
      dependencyTags = M.empty
    }

buildGraph :: Has Diagnostics sig m => [StackDep] -> m (G.Graphing Dependency)
buildGraph deps = do
  result <- fromEither =<< withMapping ignorePackageName (traverse doGraph deps)
  pure . G.gmap toDependency $ G.filter shouldInclude result

analyze :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m (G.Graphing Dependency)
analyze dir = execJson @[StackDep] dir stackJSONDepsCmd >>= buildGraph
