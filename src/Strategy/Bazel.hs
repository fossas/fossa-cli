module Strategy.Bazel (
  discover,
  BazelProject (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject, analyzeProjectStaticOnly))
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  recover,
 )
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkSkipAll),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (AllowErr (Never), Command (..), Exec, execJson)
import Effect.ReadFS (ReadFS, readContentsJson, readContentsParser)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, Dir, File, Path)
import Strategy.Bazel.BazelModGraph (BazelModGraphJson, buildModGraphDeps)
import Strategy.Bazel.MavenInstall (MavenInstallJson, buildMavenInstallGraph)
import Strategy.Bazel.ModuleBazel (buildBazelGraph, moduleBazelParser)
import Types (
  Dependency,
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (BazelProjectType),
  GraphBreadth (Complete, Partial),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject BazelProject]
discover = simpleDiscover findProjects mkProject BazelProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [BazelProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "MODULE.bazel" files of
    Nothing -> pure ([], WalkSkipAll)
    Just moduleBazel -> do
      let mavenInstall = findFileNamed "maven_install.json" files
          project =
            BazelProject
              { bazelDir = dir
              , bazelModuleFile = moduleBazel
              , bazelMavenInstall = mavenInstall
              }
      pure ([project], WalkSkipAll)

data BazelProject = BazelProject
  { bazelDir :: Path Abs Dir
  , bazelModuleFile :: Path Abs File
  , bazelMavenInstall :: Maybe (Path Abs File)
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON BazelProject

instance AnalyzeProject BazelProject where
  analyzeProject _ project = do
    staticResult <- context "Bazel" $ analyzeStatic project
    dynamicResult <- context "Bazel dynamic analysis" $ recover (analyzeDynamic (bazelDir project))
    case dynamicResult of
      Just dynGraph ->
        pure $
          staticResult
            { dependencyGraph = dependencyGraph staticResult <> dynGraph
            , dependencyGraphBreadth = Complete
            }
      Nothing -> pure staticResult

  analyzeProjectStaticOnly _ = context "Bazel" . analyzeStatic

mkProject :: BazelProject -> DiscoveredProject BazelProject
mkProject project =
  DiscoveredProject
    { projectType = BazelProjectType
    , projectBuildTargets = mempty
    , projectPath = bazelDir project
    , projectData = project
    }

-- | Static analysis: parse MODULE.bazel and optionally maven_install.json.
analyzeStatic :: (Has ReadFS sig m, Has Diagnostics sig m) => BazelProject -> m DependencyResults
analyzeStatic project = do
  moduleFile <- context "Parsing MODULE.bazel" $ readContentsParser moduleBazelParser (bazelModuleFile project)
  let bazelGraph = buildBazelGraph moduleFile
  mavenGraph <- case bazelMavenInstall project of
    Just installFile -> do
      installJson <- context "Parsing maven_install.json" $ readContentsJson @MavenInstallJson installFile
      pure $ buildMavenInstallGraph installJson
    Nothing -> pure mempty
  let combinedGraph = bazelGraph <> mavenGraph
      breadth = case bazelMavenInstall project of
        Just _ -> Complete
        Nothing -> Partial
      manifestFiles = [bazelModuleFile project] <> maybe [] pure (bazelMavenInstall project)
  pure
    DependencyResults
      { dependencyGraph = combinedGraph
      , dependencyGraphBreadth = breadth
      , dependencyManifestFiles = manifestFiles
      }

-- | Dynamic analysis: run `bazel mod graph --output json` and parse the result.
analyzeDynamic :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m (Graphing Dependency)
analyzeDynamic dir = do
  let cmd = Command "bazel" ["mod", "graph", "--output", "json"] Never
  modGraph <- execJson @BazelModGraphJson dir cmd
  pure $ buildModGraphDeps modGraph
