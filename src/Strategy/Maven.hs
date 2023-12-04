{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Strategy.Maven (
  discover,
  mkProject,
  MavenProject (..),
  getDeps,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProject'), analyzeProject)
import App.Pathfinder.Types (LicenseAnalyzeProject, licenseAnalyzeProject)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, warnOnErr, (<||>))
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Data.Aeson (ToJSON)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.NonEmpty (nonEmpty, toSet)
import Data.Text hiding (group)
import DepTypes (Dependency)
import Diag.Common (MissingDeepDeps (MissingDeepDeps), MissingEdges (MissingEdges))
import Discovery.Filters (AllFilters, MavenScopeFilters)
import Discovery.Simple (simpleDiscover)
import Effect.Exec (CandidateCommandEffs)
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Graphing (Graphing, gmap, toAdjacencyMap, vertexList)
import Path (Abs, Dir, Path, parent)
import Strategy.Maven.Common (MavenDependency (..), filterMavenDependencyByScope, filterMavenSubmodules, mavenDependencyToDependency)
import Strategy.Maven.DepTree qualified as DepTreeCmd
import Strategy.Maven.PluginStrategy qualified as Plugin
import Strategy.Maven.Pom qualified as Pom
import Strategy.Maven.Pom.Closure (MavenProjectClosure)
import Strategy.Maven.Pom.Closure qualified as PomClosure

import Effect.Logger (Logger, Pretty (pretty), logDebug, runLogger)
import Text.Pretty.Simple (pShow)
import Types (BuildTarget (..), DependencyResults (..), DiscoveredProject (..), DiscoveredProjectType (MavenProjectType), FoundTargets (..), GraphBreadth (..))

import Data.Map qualified as Map
import Debug.Trace (traceM)
import Effect.Logger (Logger, Pretty (pretty), logDebug, runLogger)
import Strategy.Maven.Pom.PomFile (MavenCoordinate (MavenCoordinate))

discover ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has (Reader AllFilters) sig m
  , Has Logger sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject MavenProject]
discover = do
  -- logDebug $ "Maven Project Closure" <> pretty (pShow (findProjects mkProject MavenProjectType))
  simpleDiscover findProjects mkProject MavenProjectType
  where
    findProjects dir = Prelude.map MavenProject <$> PomClosure.findProjects dir

mkProject :: MavenProject -> DiscoveredProject MavenProject
mkProject (MavenProject closure) = do
  DiscoveredProject
    { projectType = MavenProjectType
    , projectPath = parent $ PomClosure.closurePath closure
    , -- , projectBuildTargets = maybe ProjectWithoutTargets FoundTargets $ nonEmpty $ Set.map BuildTarget testSet
      projectBuildTargets = maybe ProjectWithoutTargets FoundTargets $ nonEmpty $ Set.map BuildTarget $ PomClosure.closureSubmodules closure
    , projectData = MavenProject closure
    }

newtype MavenProject = MavenProject {unMavenProject :: PomClosure.MavenProjectClosure}
  deriving (Eq, Ord, Show, Generic)

instance ToJSON MavenProject

instance AnalyzeProject MavenProject where
  analyzeProject = getDeps
  analyzeProject' _ = getDeps'

instance LicenseAnalyzeProject MavenProject where
  licenseAnalyzeProject = pure . Pom.getLicenses . unMavenProject

getDeps ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , CandidateCommandEffs sig m
  , Has Logger sig m
  , Has Logger sig m
  , Has (Reader MavenScopeFilters) sig m
  ) =>
  FoundTargets ->
  MavenProject ->
  m DependencyResults
getDeps foundTargets (MavenProject closure) = do
  let targetSet :: Set.Set Text
      targetSet = case foundTargets of
        FoundTargets targets -> Set.map unBuildTarget (toSet targets)
        _ -> Set.empty

  logDebug $ "Targets in get Deps for Maven " <> pretty (pShow (targetSet))
  logDebug $ "Closure submodule set " <> pretty (pShow (PomClosure.closureSubmodules closure))
  -- (graph, graphBreadth) <- context "Maven" $ getDepsDynamicAnalysis closure <||> getStaticAnalysis closure
  (graph, graphBreadth) <- context "Maven" $ getDepsDynamicAnalysis targetSet closure

  -- submodulesToDeleteGraph <- filterMavenSubmodules targetSet (PomClosure.closureSubmodules closure) graph
  logDebug $ "This is the Graph ((((((()))))))" <> pretty (pShow (graph))
  -- logDebug $ "This is the Graph after filtering submodules **********" <> pretty (pShow (submodulesToDeleteGraph))
  -- logDebug $ "This is vertex lsit of nodes to delete ((((((()))))))" <> pretty (pShow (vertexList submodulesToDeleteGraph))
  -- logDebug $ "This is the adjacency Map ((((((()))))))" <> pretty (pShow (Graphing.toAdjacencyMap graph))

  pure $
    DependencyResults
      { dependencyGraph = filteredGraph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [PomClosure.closurePath closure]
      }

getDeps' ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader MavenScopeFilters) sig m
  , Has Logger sig m
  ) =>
  MavenProject ->
  m DependencyResults
getDeps' (MavenProject closure) = do
  (graph, graphBreadth) <- context "Maven" $ getStaticAnalysis closure
  filteredGraph <- withScopeFiltering graph

  pure $
    DependencyResults
      { dependencyGraph = filteredGraph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [PomClosure.closurePath closure]
      }

getDepsDynamicAnalysis ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , CandidateCommandEffs sig m
  , Has Logger sig m
  ) =>
  Set Text ->
  MavenProjectClosure ->
  m (Graphing MavenDependency, GraphBreadth)
getDepsDynamicAnalysis closure = do
  context "Dynamic Analysis"
    $ warnOnErr MissingEdges
      . warnOnErr MissingDeepDeps
    $ getDepsTreeCmd submoduleFilters closure

-- (getDepsPlugin closure <||> getDepsTreeCmd closure <||> getDepsPluginLegacy closure)

getDepsPlugin ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Logger sig m
  ) =>
  MavenProjectClosure ->
  m (Graphing MavenDependency, GraphBreadth)
getDepsPlugin closure = context "Plugin analysis" (Plugin.analyze' . parent $ PomClosure.closurePath closure)

getDepsPluginLegacy ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  MavenProjectClosure ->
  m (Graphing MavenDependency, GraphBreadth)
getDepsPluginLegacy closure = context "Legacy Plugin analysis" (Plugin.analyzeLegacy' . parent $ PomClosure.closurePath closure)

getDepsTreeCmd ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , CandidateCommandEffs sig m
  , Has Logger sig m
  ) =>
  Set Text ->
  MavenProjectClosure ->
  m (Graphing MavenDependency, GraphBreadth)
getDepsTreeCmd closure = do
  context "Dynamic analysis" $
    DepTreeCmd.analyze submoduleFilters (PomClosure.closureSubmodules closure) . parent $
      PomClosure.closurePath closure

getStaticAnalysis ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  ) =>
  Set Text ->
  MavenProjectClosure ->
  m (Graphing MavenDependency, GraphBreadth)
getStaticAnalysis closure = do
  context "Static analysis" $ pure (Pom.analyze' closure, Partial)

withScopeFiltering :: Has (Reader MavenScopeFilters) sig m => Graphing MavenDependency -> m (Graphing Dependency)
withScopeFiltering graph = do
  mavenScopeFilters <- ask @(MavenScopeFilters)
  pure $ gmap mavenDependencyToDependency $ filterMavenDependencyByScope mavenScopeFilters graph
