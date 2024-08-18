module Strategy.Gomodules (
  discover,
  findProjects,
  getDeps,
  mkProject,
  GomodulesProject (..),
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import App.Fossa.Config.Analyze (ExperimentalAnalyzeConfig (useV3GoResolver), GoDynamicTactic (..), StrictMode (..))
import Control.Carrier.Diagnostics (warn)
import Control.Effect.Diagnostics (Diagnostics, context, fatalText, recover, (<||>))
import Control.Effect.Reader (Reader, ask, asks)
import Control.Monad (when)
import Data.Aeson (ToJSON)
import Data.Flag (Flag, fromFlag)
import Data.Text (Text)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkSkipSome),
  findFileNamed,
  walkWithFilters',
 )
import Effect.Exec (Exec, Has)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, Dir, File, Path)
import Strategy.Go.GoListPackages qualified as GoListPackages
import Strategy.Go.Gomod qualified as Gomod
import Strategy.Go.Gostd (GoStdlibDep, filterGoStdlibPackages, listGoStdlibPackages)
import Types (
  Dependency,
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (GomodProjectType),
  GraphBreadth,
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject GomodulesProject]
discover = simpleDiscover findProjects mkProject GomodProjectType

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [GomodulesProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "go.mod" files of
    Nothing -> pure ([], WalkSkipSome ["vendor"])
    Just gomod -> pure ([GomodulesProject gomod dir], WalkSkipSome ["vendor"])

data GomodulesProject = GomodulesProject
  { gomodulesGomod :: Path Abs File
  , gomodulesDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON GomodulesProject

instance AnalyzeProject GomodulesProject where
  analyzeProject _ proj = asks useV3GoResolver >>= getDeps proj
  analyzeProjectStaticOnly _ = const $ fatalText "Cannot analyze GoModule project statically"

mkProject :: GomodulesProject -> DiscoveredProject GomodulesProject
mkProject project =
  DiscoveredProject
    { projectType = GomodProjectType
    , projectBuildTargets = mempty
    , projectPath = gomodulesDir project
    , projectData = project
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m, Has (Reader (Flag StrictMode)) sig m) => GomodulesProject -> GoDynamicTactic -> m DependencyResults
getDeps project goDynamicTactic = do
  strictMode <- ask @((Flag StrictMode))
  (graph, graphBreadth) <-
    if fromFlag StrictMode strictMode
      then context "Strict mode Gomodules" dynamicAnalysis
      else context "Gomodules" $ dynamicAnalysis <||> staticAnalysis
  stdlib <- recover . context "Collect go standard library information" . listGoStdlibPackages $ gomodulesDir project
  pure $
    DependencyResults
      { dependencyGraph = filterGraph stdlib graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [gomodulesGomod project]
      }
  where
    -- `go list -json -deps all` marks std lib deps with a boolean, so Strategy.Go.GoListPackages does this filtering itself.
    -- I think this can be removed when `go list -json -deps all` becomes the default.
    filterGraph :: Maybe [GoStdlibDep] -> Graphing Dependency -> Graphing Dependency
    filterGraph Nothing deps = deps
    filterGraph (Just stdlib) deps = filterGoStdlibPackages stdlib deps

    staticAnalysis :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => m (Graphing Dependency, GraphBreadth)
    staticAnalysis = context "Static analysis" (Gomod.analyze' (gomodulesGomod project))

    dynamicAnalysis :: (Has Exec sig m, Has Diagnostics sig m) => m (Graphing Dependency, GraphBreadth)
    dynamicAnalysis =
      context "Dynamic analysis" $ do
        when (goDynamicTactic == GoPackagesBasedTactic) $
          warn @Text
            "--experimental-use-v3-go-resolver is now deprecated because the v3 resolver is the default. \
            \This option will be removed in a future release and result in an error."

        context "analysis using go list (V3 Resolver)" (GoListPackages.analyze (gomodulesDir project))
