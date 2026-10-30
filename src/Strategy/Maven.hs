module Strategy.Maven (
  discover,
  discoverVisitor,
  postDiscover,
  mkProject,
  MavenProject (..),
  getDeps,
  getDepsStatically,
) where

import App.Fossa.Analyze.LicenseAnalyze (LicenseAnalyzeProject, licenseAnalyzeProject)
import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import App.Types (Mode (..))
import App.Util (guardStrictMode)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, warnOnErr, (<||>))
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Data.Aeson (ToJSON)
import Data.List (isSuffixOf)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.NonEmpty (nonEmpty, toSet)
import Data.Text hiding (filter, group, isSuffixOf, map)
import DepTypes (Dependency)
import Diag.Common (MissingDeepDeps (MissingDeepDeps), MissingEdges (MissingEdges))
import Discovery.Filters (AllFilters, MavenScopeFilters, mavenScopeFilterSet)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (WalkStep (WalkSkipSome), fileName)
import Effect.Exec (CandidateCommandEffs, GetDepsEffs)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Graphing (Graphing, gmap, shrinkRoots)
import Path (Abs, Dir, File, Path, parent)
import Strategy.Maven.Common (MavenDependency (..), filterMavenDependencyByScope, filterMavenSubmodules, mavenDependencyToDependency, promoteFirstPartyToDirect)
import Strategy.Maven.DepTree qualified as DepTreeCmd
import Strategy.Maven.PluginStrategy qualified as Plugin
import Strategy.Maven.Pom qualified as Pom
import Strategy.Maven.Pom.Closure (MavenProjectClosure (..))
import Strategy.Maven.Pom.Closure qualified as PomClosure
import Strategy.Maven.Pom.Resolver (buildGlobalClosure)
import Types (BuildTarget (..), DependencyResults (..), DiscoveredProject (..), DiscoveredProjectType (MavenProjectType), FoundTargets (..), GraphBreadth (..))

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

-- | Per-directory collector for the shared discovery walk: gather @pom.xml@
-- and @.pom@ paths, skipping @target/@ subtrees — the same candidate set the
-- private walk in "Strategy.Maven.Pom.Closure.findProjects" collects.
discoverVisitor :: (Has ReadFS sig m) => Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m ([Path Abs File], WalkStep)
discoverVisitor _ _ files = pure (filter isPom files, WalkSkipSome ["target"])
  where
    isPom file = "pom.xml" `isSuffixOf` fileName file || ".pom" `isSuffixOf` fileName file

-- | Batch phase for the shared discovery walk: build the global pom closure
-- and per-project closures over the whole collected pom set (the same
-- algorithm "Strategy.Maven.Pom.Closure.findProjects" runs).
postDiscover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> [Path Abs File] -> m [DiscoveredProject MavenProject]
postDiscover dir pomFiles = do
  globalClosure <- context "Building global closure" $ buildGlobalClosure pomFiles
  closures <- context "Building project closures" $ PomClosure.buildProjectClosures dir globalClosure
  pure $ map (mkProject . MavenProject) closures

mkProject :: MavenProject -> DiscoveredProject MavenProject
mkProject (MavenProject closure) =
  DiscoveredProject
    { projectType = MavenProjectType
    , projectPath = parent $ PomClosure.closurePath closure
    , projectBuildTargets = buildTargets
    , projectData = MavenProject closure
    }
  where
    buildTargets :: FoundTargets
    buildTargets = maybe ProjectWithoutTargets FoundTargets $ nonEmpty (Set.map BuildTarget $ PomClosure.closureSubmodules closure)

newtype MavenProject = MavenProject {unMavenProject :: PomClosure.MavenProjectClosure}
  deriving (Eq, Ord, Show, Generic)

instance ToJSON MavenProject

instance AnalyzeProject MavenProject where
  analyzeProject = getDeps
  analyzeProjectStaticOnly = getDepsStatically

instance LicenseAnalyzeProject MavenProject where
  licenseAnalyzeProject = pure . Pom.getLicenses . unMavenProject

getDeps ::
  ( Has (Lift IO) sig m
  , GetDepsEffs sig m
  , CandidateCommandEffs sig m
  , Has (Reader MavenScopeFilters) sig m
  ) =>
  FoundTargets ->
  MavenProject ->
  m DependencyResults
getDeps foundTargets (MavenProject closure) = do
  let submoduleTargets = submoduleTargetSet foundTargets
  mode <- ask
  (graph, graphBreadth) <-
    context "Maven" $
      getDepsDynamicAnalysis submoduleTargets closure
        <||> guardStrictMode mode (getStaticAnalysis submoduleTargets closure)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [PomClosure.closurePath closure]
      }

getDepsStatically ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader MavenScopeFilters) sig m
  ) =>
  FoundTargets ->
  MavenProject ->
  m DependencyResults
getDepsStatically foundTargets (MavenProject closure) = do
  let submoduleTargets = submoduleTargetSet foundTargets
  (graph, graphBreadth) <- context "Maven" $ getStaticAnalysis submoduleTargets closure

  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [PomClosure.closurePath closure]
      }

getDepsDynamicAnalysis ::
  ( Has (Lift IO) sig m
  , GetDepsEffs sig m
  , CandidateCommandEffs sig m
  , Has (Reader MavenScopeFilters) sig m
  ) =>
  Set Text ->
  MavenProjectClosure ->
  m (Graphing Dependency, GraphBreadth)
getDepsDynamicAnalysis submoduleTargets closure = do
  let allSubmodules = PomClosure.closureSubmodules closure
  mode <- ask
  let errorInfo = case mode of
        Strict -> id
        NonStrict -> warnOnErr MissingEdges . warnOnErr MissingDeepDeps
  (graph, graphBreadth) <-
    context "Dynamic Analysis" $
      errorInfo (getDepsPlugin closure <||> guardStrictMode mode (getDepsTreeCmd closure <||> getDepsPluginLegacy closure))

  finalGraph <- finalizeMavenGraph submoduleTargets allSubmodules graph
  pure (finalGraph, graphBreadth)

getDepsPlugin ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  MavenProjectClosure ->
  m (Graphing MavenDependency, GraphBreadth)
getDepsPlugin closure =
  context "Plugin analysis" $
    Plugin.analyze' (PomClosure.closurePoms closure) (parent $ PomClosure.closurePath closure)

getDepsPluginLegacy ::
  ( CandidateCommandEffs sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  MavenProjectClosure ->
  m (Graphing MavenDependency, GraphBreadth)
getDepsPluginLegacy closure =
  context "Legacy Plugin analysis" $
    Plugin.analyzeLegacy' (PomClosure.closurePoms closure) (parent $ PomClosure.closurePath closure)

getDepsTreeCmd ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , CandidateCommandEffs sig m
  ) =>
  MavenProjectClosure ->
  m (Graphing MavenDependency, GraphBreadth)
getDepsTreeCmd closure =
  context "Dynamic analysis" $
    DepTreeCmd.analyze . parent $
      PomClosure.closurePath closure

getStaticAnalysis ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader MavenScopeFilters) sig m
  ) =>
  Set Text ->
  MavenProjectClosure ->
  m (Graphing Dependency, GraphBreadth)
getStaticAnalysis submoduleTargets closure = do
  let allSubmodules = PomClosure.closureSubmodules closure
  (graph, graphBreadth) <- context "Static analysis" $ pure (Pom.analyze' closure, Partial)
  -- Pom.analyze' marks the project's own coordinate as the sole direct node.
  -- Mirror the dynamic path's buildGraph by additionally marking submodules
  -- direct; finalizeMavenGraph's shrinkRoots then removes all first-party
  -- artifacts and promotes their declared dependencies to direct.
  let withFirstPartyAsRoots = promoteFirstPartyToDirect allSubmodules graph
  finalGraph <- finalizeMavenGraph submoduleTargets allSubmodules withFirstPartyAsRoots
  pure (finalGraph, graphBreadth)

-- | Shared final step for every Maven graph strategy: apply submodule/scope
-- filtering, then remove the users' own packages (the toplevel package or
-- submodules in a multi-module project, which each strategy marks as graph
-- roots) and promote their children to direct via `shrinkRoots`. Centralizing
-- this keeps the static and dynamic paths from diverging; the static path
-- previously omitted this step, which reported the project artifact as the
-- sole Direct dependency.
finalizeMavenGraph ::
  (Has Diagnostics sig m, Has (Reader MavenScopeFilters) sig m) =>
  Set Text ->
  Set Text ->
  Graphing MavenDependency ->
  m (Graphing Dependency)
finalizeMavenGraph targetSet submoduleSet graph = do
  filteredGraph <- applyMavenFilters targetSet submoduleSet graph
  pure (shrinkRoots filteredGraph)

applyMavenFilters :: (Has Diagnostics sig m, Has (Reader MavenScopeFilters) sig m) => Set Text -> Set Text -> Graphing MavenDependency -> m (Graphing Dependency)
applyMavenFilters targetSet submoduleSet graph = do
  mavenScopeFilters <- ask @(MavenScopeFilters)
  filteredSubmoduleGraph <-
    if targetSet == submoduleSet
      then pure graph
      else context "Filter maven submodules" $ pure (filterMavenSubmodules targetSet submoduleSet graph)
  filteredSubmoduleScopeGraph <-
    if Set.null $ mavenScopeFilterSet mavenScopeFilters
      then pure filteredSubmoduleGraph
      else context "Filter maven scopes" $ pure (filterMavenDependencyByScope submoduleSet mavenScopeFilters filteredSubmoduleGraph)
  pure $ gmap mavenDependencyToDependency filteredSubmoduleScopeGraph

submoduleTargetSet :: FoundTargets -> Set Text
submoduleTargetSet foundTargets = case foundTargets of
  FoundTargets targets -> Set.map unBuildTarget (toSet targets)
  _ -> Set.empty
