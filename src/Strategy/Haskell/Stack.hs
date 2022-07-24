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
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  fromEither,
 )
import Control.Effect.Reader (Reader)
import Control.Monad (when)
import Data.Aeson.Types (
  FromJSON (parseJSON),
  ToJSON,
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.String.Conversion (toString)
import Data.Text (Text)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipAll),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (AllowErr (Never), Command (..), Exec, execJson)
import Effect.Grapher (
  MappedGrapher,
  direct,
  edge,
  mapping,
  withMapping,
 )
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Graphing qualified as G
import Path (Abs, Dir, File, Path)
import Types (
  DepType (HackageType),
  Dependency (..),
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (StackProjectType),
  GraphBreadth (Complete),
  VerConstraint (CEq),
 )
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
    StackDep
      <$> obj .: "name"
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

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject StackProject]
discover = simpleDiscover findProjects mkProject StackProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [StackProject]
findProjects = walkWithFilters' $ \dir _ files -> do
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
