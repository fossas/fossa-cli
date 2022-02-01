{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Strategy.Haskell.Stack (
  discover,

  -- * Testing
  buildGraph,
  PackageName (..),
  StackDep (..),
  StackLocation (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Effect.Diagnostics
import Control.Monad (when)
import Data.Aeson.Types
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.String.Conversion (toString)
import Data.Text (Text)
import Discovery.Walk
import Effect.Exec
import Effect.Grapher
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Graphing qualified as G
import Path
import Types
import Prelude

newtype PackageName = PackageName {unPackageName :: Text} deriving (FromJSON, Eq, Ord, Show)

data StackDep = StackDep
  { stackName :: PackageName
  , stackVersion :: Text
  , stackDepNames :: [PackageName]
  , stackLocation :: StackLocation
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
  | otherwise = fail $ "Bad location type: " ++ toString txt

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject StackProject]
discover dir = context "Stack" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [StackProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "stack.yaml" files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([StackProject dir file], WalkSkipAll)

mkProject :: StackProject -> DiscoveredProject StackProject
mkProject project =
  DiscoveredProject
    { projectType = StackProjectType
    , projectBuildTargets = mempty
    , projectPath = stackDir project
    , projectData = project
    }

getDeps :: (Has Exec sig m, Has Diagnostics sig m) => StackProject -> m DependencyResults
getDeps project =
  context "Stack" $
    context "Dynamic analysis" $
      analyze project

data StackProject = StackProject
  { stackDir :: Path Abs Dir
  , stackFile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON StackProject

instance AnalyzeProject StackProject where
  analyzeProject _ = getDeps

stackJSONDepsCmd :: Command
stackJSONDepsCmd =
  Command
    { cmdName = "stack"
    , cmdArgs = ["ls", "dependencies", "json"]
    , cmdAllowErr = Never
    }

doGraph :: Has (MappedGrapher PackageName StackDep) sig m => StackDep -> m ()
doGraph dep = do
  let name = stackName dep
  mapping name dep
  for_ (stackDepNames dep) $ \child -> do
    edge name child
    when (stackLocation dep == Local) (direct child)

ignorePackageName :: PackageName -> a -> a
ignorePackageName _ x = x

shouldInclude :: StackDep -> Bool
shouldInclude dep = Remote == stackLocation dep

toDependency :: StackDep -> Dependency
toDependency dep =
  Dependency
    { dependencyType = HackageType
    , dependencyName = unPackageName $ stackName dep
    , dependencyVersion = Just $ CEq $ stackVersion dep
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

buildGraph :: Has Diagnostics sig m => [StackDep] -> m (G.Graphing Dependency)
buildGraph deps = do
  result <- fromEither =<< withMapping ignorePackageName (traverse doGraph deps)
  pure . G.gmap toDependency $ G.filter shouldInclude result

analyze :: (Has Exec sig m, Has Diagnostics sig m) => StackProject -> m DependencyResults
analyze project = do
  graph <- execJson @[StackDep] (stackDir project) stackJSONDepsCmd >>= buildGraph
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [stackFile project]
      }
