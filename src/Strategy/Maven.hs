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
import DepTypes (Dependency)
import Diag.Common (MissingDeepDeps (MissingDeepDeps), MissingEdges (MissingEdges))
import Discovery.Filters (AllFilters, MavenScopeFilters)
import Discovery.Simple (simpleDiscover)
import Effect.Exec (CandidateCommandEffs)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Graphing (Graphing, gmap)
import Path (Abs, Dir, Path, parent)
import Strategy.Maven.Common (MavenDependency (..), filterMavenDependencyByScope, mavenDependencyToDependency)
import Strategy.Maven.DepTree qualified as DepTreeCmd
import Strategy.Maven.PluginStrategy qualified as Plugin
import Strategy.Maven.Pom qualified as Pom
import Strategy.Maven.Pom.Closure (MavenProjectClosure)
import Strategy.Maven.Pom.Closure qualified as PomClosure
import Types (DependencyResults (..), DiscoveredProject (..), DiscoveredProjectType (MavenProjectType), GraphBreadth (..))

discover ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject MavenProject]
discover = do
  simpleDiscover findProjects mkProject MavenProjectType
  where
    findProjects dir = map MavenProject <$> PomClosure.findProjects dir

mkProject :: MavenProject -> DiscoveredProject MavenProject
mkProject (MavenProject closure) =
  DiscoveredProject
    { projectType = MavenProjectType
    , projectPath = parent $ PomClosure.closurePath closure
    , projectBuildTargets = mempty
    , projectData = MavenProject closure
    }

newtype MavenProject = MavenProject {unMavenProject :: PomClosure.MavenProjectClosure}
  deriving (Eq, Ord, Show, Generic)

instance ToJSON MavenProject

instance AnalyzeProject MavenProject where
  analyzeProject _ = getDeps
  analyzeProject' _ = getDeps'

instance LicenseAnalyzeProject MavenProject where
  licenseAnalyzeProject = pure . Pom.getLicenses . unMavenProject

getDeps ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , CandidateCommandEffs sig m
  , Has (Reader MavenScopeFilters) sig m
  ) =>
  MavenProject ->
  m DependencyResults
getDeps (MavenProject closure) = do
  (graph, graphBreadth) <- context "Maven" $ getDepsDynamicAnalysis closure <||> getStaticAnalysis closure
  filteredGraph <- withScopeFiltering graph

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
  ) =>
  MavenProjectClosure ->
  m (Graphing MavenDependency, GraphBreadth)
getDepsDynamicAnalysis closure = do
  context "Dynamic Analysis"
    $ warnOnErr MissingEdges
      . warnOnErr MissingDeepDeps
    $ (getDepsPlugin closure <||> getDepsTreeCmd closure <||> getDepsPluginLegacy closure)

getDepsPlugin ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
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
  ) =>
  MavenProjectClosure ->
  m (Graphing MavenDependency, GraphBreadth)
getDepsTreeCmd closure = do
  context "Dynamic analysis" $
    DepTreeCmd.analyze . parent $
      PomClosure.closurePath closure

getStaticAnalysis ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  ) =>
  MavenProjectClosure ->
  m (Graphing MavenDependency, GraphBreadth)
getStaticAnalysis closure = do
  context "Static analysis" $ pure (Pom.analyze' closure, Partial)

withScopeFiltering :: Has (Reader MavenScopeFilters) sig m => Graphing MavenDependency -> m (Graphing Dependency)
withScopeFiltering graph = do
  mavenScopeFilters <- ask @(MavenScopeFilters)
  pure $ gmap mavenDependencyToDependency $ filterMavenDependencyByScope mavenScopeFilters graph
