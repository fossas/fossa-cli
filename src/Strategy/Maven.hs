module Strategy.Maven (
  discover,
  mkProject,
  MavenProject (..),
  getDeps,
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import App.Pathfinder.Types (LicenseAnalyzeProject, licenseAnalyzeProject)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, warnOnErr, (<||>))
import Control.Effect.Lift (Lift)
import Data.Aeson (ToJSON)
import Diag.Common (MissingDeepDeps (MissingDeepDeps), MissingEdges (MissingEdges))
import Effect.Exec (Exec)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, Dir, Path, parent)
import Strategy.Maven.DepTree qualified as DepTreeCmd
import Strategy.Maven.PluginStrategy qualified as Plugin
import Strategy.Maven.Pom qualified as Pom
import Strategy.Maven.Pom.Closure (MavenProjectClosure)
import Strategy.Maven.Pom.Closure qualified as PomClosure
import Types (Dependency, DependencyResults (..), DiscoveredProject (..), DiscoveredProjectType (MavenProjectType), GraphBreadth (..))

discover ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject MavenProject]
discover dir = context "Maven" $ do
  closures <- context "Finding projects" (PomClosure.findProjects dir)
  pure (map mkProject closures)

mkProject ::
  PomClosure.MavenProjectClosure ->
  DiscoveredProject MavenProject
mkProject closure =
  DiscoveredProject
    { projectType = MavenProjectType
    , projectPath = parent $ PomClosure.closurePath closure
    , projectBuildTargets = mempty
    , projectData = MavenProject closure
    -- , projectDependencyResults = const $ getDeps closure
    -- , projectLicenses = pure $ Pom.getLicenses basedir closure
    }

newtype MavenProject = MavenProject {unMavenProject :: PomClosure.MavenProjectClosure}
  deriving (Eq, Ord, Show, Generic)

instance ToJSON MavenProject

instance AnalyzeProject MavenProject where
  analyzeProject _ = getDeps

instance LicenseAnalyzeProject MavenProject where
  licenseAnalyzeProject = pure . Pom.getLicenses . unMavenProject

getDeps ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  MavenProject ->
  m DependencyResults
getDeps (MavenProject closure) = do
  (graph, graphBreadth) <- context "Maven" $ getDepsDynamicAnalysis closure <||> getStaticAnalysis closure
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [PomClosure.closurePath closure]
      }

getDepsDynamicAnalysis ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  MavenProjectClosure ->
  m (Graphing Dependency, GraphBreadth)
getDepsDynamicAnalysis closure =
  context "Dynamic Analysis" $
    warnOnErr MissingEdges
      . warnOnErr MissingDeepDeps
      $ (getDepsPlugin closure <||> getDepsTreeCmd closure)

getDepsPlugin ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  MavenProjectClosure ->
  m (Graphing Dependency, GraphBreadth)
getDepsPlugin closure = context "Plugin analysis" (Plugin.analyze' . parent $ PomClosure.closurePath closure)

getDepsTreeCmd ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  MavenProjectClosure ->
  m (Graphing Dependency, GraphBreadth)
getDepsTreeCmd closure =
  context "Dynamic analysis" $
    DepTreeCmd.analyze . parent $ PomClosure.closurePath closure

getStaticAnalysis ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  ) =>
  MavenProjectClosure ->
  m (Graphing Dependency, GraphBreadth)
getStaticAnalysis closure = context "Static analysis" $ pure (Pom.analyze' closure, Partial)
