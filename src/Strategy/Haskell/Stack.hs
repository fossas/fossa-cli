{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Strategy.Haskell.Stack (
  discover,

  -- * Testing
  buildGraph,
  GitUrl (..),
  GitSha (..),
  PackageName (..),
  StackDep (..),
  StackLocation (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  fatalText,
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
import Data.Maybe (mapMaybe)
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
  DepType (GitType, HackageType),
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

newtype GitUrl = GitUrl {unGitUrl :: Text}
  deriving (Eq, Ord, Show, FromJSON)

newtype GitSha = GitSha {unGitSha :: Text}
  deriving (Eq, Ord, Show, FromJSON)

data StackLocation
  = Local
  | Remote
  | Git GitUrl GitSha
  | BuiltIn
  deriving (Eq, Ord, Show)

instance FromJSON StackDep where
  parseJSON =
    withObject "StackDep" $ \obj -> do
      location <- obj .:? "location" .!= BuiltIn
      version <- case location of
        Git _ sha -> pure . unGitSha $ sha
        _ -> obj .: "version"
      StackDep
        <$> obj .: "name"
        <*> pure version
        <*> obj .:? "dependencies" .!= []
        <*> pure location

instance FromJSON StackLocation where
  parseJSON = withObject "StackLocation" $ \obj ->
    do
      (ty :: Text) <- obj .: "type"
      case ty of
        "git" -> Git <$> obj .: "url" <*> obj .: "commit"
        "hackage" -> pure Remote
        txt
          | txt `elem` ["project package", "archive"] -> pure Local
          | otherwise -> fail $ "Bad location type: " ++ toString txt

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
  analyzeProjectStaticOnly _ = const $ fatalText "Cannot analyze stack project statically"

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
shouldInclude dep = case stackLocation dep of
  Remote -> True
  Git _ _ -> True
  _ -> False

toDependency :: StackDep -> Dependency
toDependency dep =
  Dependency
    { dependencyType = depType
    , dependencyName = unPackageName $ stackName dep
    , dependencyVersion = Just $ CEq $ stackVersion dep
    , dependencyLocations = locations
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }
  where
    (locations, depType) =
      case stackLocation dep of
        Git url _ -> ([unGitUrl url], GitType)
        _ -> ([], HackageType)

buildGraph :: Has Diagnostics sig m => [StackDep] -> m (G.Graphing Dependency)
buildGraph deps = do
  let deps' = map remapHackageToGitNames deps
  result <- fromEither =<< withMapping ignorePackageName (traverse doGraph deps')
  pure . G.gmap toDependency $ G.filter shouldInclude result
  where
    -- Packages in a stack project can be git repos rather than hackage packages.
    -- However, libraries depending on them will refer to them with their hackage name in the parsed `StackDep`s.
    -- For example pkg instead of https://github.com/name/pkg.
    -- Git dependencies need to have their name be a url in order to output a correct locator.
    -- The following will map a 'StackDep's name to a git url where appropriate and do the same for its child dependencies.
    remapHackageToGitNames :: StackDep -> StackDep
    remapHackageToGitNames s@StackDep{stackLocation = Git uri _} = replaceChildDepNames s{stackName = PackageName . unGitUrl $ uri}
    remapHackageToGitNames s = replaceChildDepNames s

    replaceChildDepNames :: StackDep -> StackDep
    replaceChildDepNames s@StackDep{stackDepNames = childDeps} = s{stackDepNames = (\name -> Map.findWithDefault name name hackageNamesToGitRepoNames) <$> childDeps}

    hackageNamesToGitRepoNames :: Map.Map PackageName PackageName
    hackageNamesToGitRepoNames =
      Map.fromList
        . mapMaybe
          ( \case
              StackDep{stackLocation = Git url _, stackName = name} -> Just (name, PackageName . unGitUrl $ url)
              _ -> Nothing
          )
        $ deps

analyze :: (Has Exec sig m, Has Diagnostics sig m) => StackProject -> m DependencyResults
analyze project = do
  graph <- execJson @[StackDep] (stackDir project) stackJSONDepsCmd >>= buildGraph
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [stackFile project]
      }
