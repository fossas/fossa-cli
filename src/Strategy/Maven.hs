{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
import Data.Set qualified as Set
import Data.Set.NonEmpty (nonEmpty, toSet)
import Data.Text hiding (group)
import DepTypes (Dependency)
import Diag.Common (MissingDeepDeps (MissingDeepDeps), MissingEdges (MissingEdges))
import Discovery.Filters (AllFilters, MavenScopeFilters)
import Discovery.Simple (simpleDiscover)
import Effect.Exec (CandidateCommandEffs)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Graphing (Graphing, gmap, shrinkRoots)
import Path (Abs, Dir, Path, parent)
import Strategy.Maven.Common (MavenDependency (..), filterMavenDependencyByScope, filterMavenSubmodules, mavenDependencyToDependency)
import Strategy.Maven.DepTree qualified as DepTreeCmd
import Strategy.Maven.PluginStrategy qualified as Plugin
import Strategy.Maven.Pom qualified as Pom
import Strategy.Maven.Pom.Closure (MavenProjectClosure (..))
import Strategy.Maven.Pom.Closure qualified as PomClosure

import Algebra.Graph.AdjacencyIntMap ()
import Data.Set (Set)
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
  -- logDebug $ "Maven Project Closure" <> pretty (pShow (findProjects mkProject MavenProjectType))
  simpleDiscover findProjects mkProject MavenProjectType
  where
    findProjects dir = Prelude.map MavenProject <$> PomClosure.findProjects dir

mkProject :: MavenProject -> DiscoveredProject MavenProject
mkProject (MavenProject closure) = do
  DiscoveredProject
    { projectType = MavenProjectType
    , projectPath = parent $ PomClosure.closurePath closure
    , projectBuildTargets = maybe ProjectWithoutTargets FoundTargets $ nonEmpty $ Set.map BuildTarget $ PomClosure.closureSubmodules closure
    , projectData = MavenProject closure
    }

newtype MavenProject = MavenProject {unMavenProject :: PomClosure.MavenProjectClosure}
  deriving (Eq, Ord, Show, Generic)

instance ToJSON MavenProject

instance AnalyzeProject MavenProject where
  analyzeProject = getDeps
  analyzeProject' = getDeps'

instance LicenseAnalyzeProject MavenProject where
  licenseAnalyzeProject = pure . Pom.getLicenses . unMavenProject

getDeps ::
  ( Has (Lift IO) sig m
  , Has ReadFS sig m
  , CandidateCommandEffs sig m
  , Has (Reader MavenScopeFilters) sig m
  ) =>
  FoundTargets ->
  MavenProject ->
  m DependencyResults
getDeps foundTargets (MavenProject closure) = do
  let submoduleTargets = submoduleTargetSet foundTargets
  (graph, graphBreadth) <- context "Maven" $ getDepsDynamicAnalysis submoduleTargets closure <||> getStaticAnalysis submoduleTargets closure

  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [PomClosure.closurePath closure]
      }

getDeps' ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader MavenScopeFilters) sig m
  ) =>
  FoundTargets ->
  MavenProject ->
  m DependencyResults
getDeps' foundTargets (MavenProject closure) = do
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
  , Has ReadFS sig m
  , CandidateCommandEffs sig m
  , Has (Reader MavenScopeFilters) sig m
  ) =>
  Set Text ->
  MavenProjectClosure ->
  m (Graphing Dependency, GraphBreadth)
getDepsDynamicAnalysis submoduleTargets closure = do
  let allSubmodules = completeSubmoduleSet closure
  (graph, graphBreadth) <-
    context "Dynamic Analysis"
      $ warnOnErr MissingEdges
        . warnOnErr MissingDeepDeps
      $ (getDepsPlugin closure <||> getDepsTreeCmd closure <||> getDepsPluginLegacy closure)
  filteredGraph <- applyMavenFilters submoduleTargets allSubmodules graph
  pure (withoutProjectAsDep filteredGraph, graphBreadth)
  where
    -- shrinkRoots is applied on all dynamic strategies. Previously, this was done downstream but now we are moving it upstream.
    -- This allows us to have reference to the submodules so that we can filter
    -- The root deps in the maven depgraph text graph output are either the
    -- toplevel package or submodules in a multi-module project. We don't want to
    -- consider those because they're the users' packages, so promote them to
    -- direct when building the graph using `shrinkRoots`.
    withoutProjectAsDep = shrinkRoots

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
  , Has (Reader MavenScopeFilters) sig m
  ) =>
  Set Text ->
  MavenProjectClosure ->
  m (Graphing Dependency, GraphBreadth)
getStaticAnalysis submoduleTargets closure = do
  let allSubmodules = completeSubmoduleSet closure
  (graph, graphBreadth) <- context "Static analysis" $ pure (Pom.analyze' closure, Partial)
  filteredGraph <- applyMavenFilters submoduleTargets allSubmodules graph
  pure (filteredGraph, graphBreadth)

applyMavenFilters :: Has (Reader MavenScopeFilters) sig m => Set Text -> Set Text -> Graphing MavenDependency -> m (Graphing Dependency)
applyMavenFilters submoduleTargetSet completeSubmoduleSet graph = do
  mavenScopeFilters <- ask @(MavenScopeFilters)
  let filteredSubmoduleGraph = filterMavenSubmodules submoduleTargetSet completeSubmoduleSet graph
      filteredSubmoduleScopeGraph = filterMavenDependencyByScope mavenScopeFilters filteredSubmoduleGraph

  pure $ gmap mavenDependencyToDependency filteredSubmoduleScopeGraph

submoduleTargetSet :: FoundTargets -> Set Text
submoduleTargetSet foundTargets = case foundTargets of
  FoundTargets targets -> Set.map unBuildTarget (toSet targets)
  _ -> Set.empty

completeSubmoduleSet :: MavenProjectClosure -> Set Text
completeSubmoduleSet MavenProjectClosure{..} = closureSubmodules