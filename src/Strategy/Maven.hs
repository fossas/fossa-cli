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
import Control.Effect.Reader (Reader, asks)
import Data.Aeson (ToJSON)
import Debug.Trace (traceM)
import Diag.Common (MissingDeepDeps (MissingDeepDeps), MissingEdges (MissingEdges))
import Discovery.Filters (AllFilters, FilterSet (scopes), MavenScopeFilters (excludeScope, includeScope))
import Discovery.Simple (simpleDiscover)
import Effect.Exec (CandidateCommandEffs)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Graphing (Graphing, gmap)
import Path (Abs, Dir, Path, parent)
import Strategy.Maven.DepTree qualified as DepTreeCmd
import Strategy.Maven.PluginStrategy qualified as Plugin
import Strategy.Maven.Pom qualified as Pom
import Strategy.Maven.Pom.Closure (MavenProjectClosure)
import Strategy.Maven.Pom.Closure qualified as PomClosure
import Text.Pretty.Simple (pShow)

import Control.Effect.Diagnostics qualified as Diag
import Data.Set (Set)
import Data.Text (Text)
import Effect.Logger (Logger, Pretty (pretty), logDebug)
import Strategy.Maven.PluginStrategy (MavenDep (..), filterMavenDepByScope)
import Types (Dependency, DependencyResults (..), DiscoveredProject (..), DiscoveredProjectType (MavenProjectType), GraphBreadth (..))

-- import Data.Text.Pretty (pShow)
-- toString . pShow $ reqs
-- Data.String.Conversion (toString)

discover ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject MavenProject]
discover = do
  traceM ("find projects in Maven.hs Discover ------ " ++ show ())
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
  , Has Logger sig m
  ) =>
  MavenProject ->
  m DependencyResults
getDeps (MavenProject closure) = do
  includeScopeFilters <- asks includeScope
  excludeScopeFilters <- asks excludeScope
  let includeScopeFilterSet = scopes includeScopeFilters
      excludeScopeFilterSet = scopes excludeScopeFilters
  traceM (" getDeps Maven.hs ----- " ++ show (MavenProject closure))
  -- logDebug $ "Maven Project Closure" <> pretty (pShow (MavenProject closure))
  -- (graph, graphBreadth) <- context "Maven" $ getDepsDynamicAnalysis closure <||> getStaticAnalysis closure
  (graph, graphBreadth) <- context "Maven" $ getStaticAnalysis closure
  logDebug $ "Old Graph" <> pretty (pShow (graph))
  -- let filteredGraph = filterMavenDepByScope includeScopeFilterSet excludeScopeFilterSet graph
  --     newGraph = gmap Plugin.mavenDepToDependency filteredGraph
  -- logDebug $ "New Maven Dep Graph" <> pretty (pShow (graph))
  -- logDebug $ "Filtered Maven Dep Graph" <> pretty (pShow (filteredGraph))
  -- logDebug $ "Maven Dep Graph -> Dependency Graph" <> pretty (pShow (newGraph))
  -- traceM (" getDeps Maven.hs -> Graph ****  ----- " ++ show (graph))
  pure $
    DependencyResults
      { dependencyGraph = graph
      , -- dependencyGraph = newGraph
        dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [PomClosure.closurePath closure]
      }

getDeps' ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  ) =>
  MavenProject ->
  m DependencyResults
getDeps' (MavenProject closure) = do
  (graph, graphBreadth) <- context "Maven" $ getStaticAnalysis closure
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [PomClosure.closurePath closure]
      }

getDepsDynamicAnalysis ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , CandidateCommandEffs sig m
  , Has Logger sig m
  ) =>
  MavenProjectClosure ->
  m (Graphing MavenDep, GraphBreadth)
getDepsDynamicAnalysis closure = do
  traceM ("In get dynamic Analysis about to perform get tree Deps 1!!!!!!!!")
  -- \$ (getDepsPlugin closure <||> getDepsTreeCmd closure <||> getDepsPluginLegacy closure)
  context "Dynamic Analysis"
    $ warnOnErr MissingEdges
      . warnOnErr MissingDeepDeps
    $ (getDepsTreeCmd closure)

getDepsPlugin ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has (Reader MavenScopeFilters) sig m
  ) =>
  MavenProjectClosure ->
  m (Graphing MavenDep, GraphBreadth)
getDepsPlugin closure = context "Plugin analysis" (Plugin.analyze' . parent $ PomClosure.closurePath closure)

getDepsPluginLegacy ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has (Reader MavenScopeFilters) sig m
  ) =>
  MavenProjectClosure ->
  m (Graphing MavenDep, GraphBreadth)
getDepsPluginLegacy closure = context "Legacy Plugin analysis" (Plugin.analyzeLegacy' . parent $ PomClosure.closurePath closure)

getDepsTreeCmd ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , CandidateCommandEffs sig m
  , Has Logger sig m
  ) =>
  MavenProjectClosure ->
  m (Graphing MavenDep, GraphBreadth)
getDepsTreeCmd closure = do
  traceM ("Get Deps Tree CMD @@@@@@@@@@")
  context "Dynamic analysis" $
    DepTreeCmd.analyze . parent $
      PomClosure.closurePath closure

getStaticAnalysis ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  ) =>
  MavenProjectClosure ->
  m (Graphing Dependency, GraphBreadth)
getStaticAnalysis closure = do
  -- traceM ("IN GET STATIC ANALYSIS ^^^^^^^")
  context "Static analysis" $ pure (Pom.analyze' closure, Partial)
