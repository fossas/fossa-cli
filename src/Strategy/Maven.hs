module Strategy.Maven (
  discover,
  mkProject,
  getDeps,
) where

import Control.Effect.Diagnostics
import Control.Effect.Lift
import Control.Monad.IO.Class
import Effect.Exec
import Effect.ReadFS
import Graphing (Graphing)
import Path
import Strategy.Maven.PluginStrategy qualified as Plugin
import Strategy.Maven.Pom qualified as Pom
import Strategy.Maven.Pom.Closure qualified as PomClosure
import Types

discover ::
  ( MonadIO m
  , Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has Exec rsig run
  , Has ReadFS rsig run
  , Has Diagnostics rsig run
  , Has (Lift IO) rsig run
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject run]
discover dir = context "Maven" $ do
  closures <- context "Finding projects" (PomClosure.findProjects dir)
  pure (map (mkProject dir) closures)

mkProject ::
  (Has ReadFS sig n, Has Exec sig n, Has (Lift IO) sig n, Has Diagnostics sig n) =>
  -- | basedir; required for licenses
  Path Abs Dir ->
  PomClosure.MavenProjectClosure ->
  DiscoveredProject n
mkProject basedir closure =
  DiscoveredProject
    { projectType = "maven"
    , projectPath = parent $ PomClosure.closurePath closure
    , projectBuildTargets = mempty
    , projectDependencyGraph = const $ getDeps closure
    , projectLicenses = pure $ Pom.getLicenses basedir closure
    }

getDeps ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has ReadFS sig m
  , Has Exec sig m
  ) =>
  PomClosure.MavenProjectClosure ->
  m (Graphing Dependency, GraphBreadth)
getDeps closure =
  context "Maven" $
    context "Plugin analysis" (Plugin.analyze' (parent (PomClosure.closurePath closure)))
      <||> context "Static analysis" (pure (Pom.analyze' closure, Partial))
