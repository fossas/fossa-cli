module Discovery.Projects (
  withDiscoveredProjects,
) where

import App.Fossa.Analyze.Debug (DiagDebugC, diagToDebug)
import Control.Carrier.Debug (Debug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Output.IO
import Control.Carrier.Stack.StickyLog
import Control.Effect.AtomicCounter (AtomicCounter)
import Control.Effect.Lift
import Control.Effect.Stack (Stack, withEmptyStack)
import Control.Effect.TaskPool
import Data.Foldable (traverse_)
import Effect.Logger
import Path
import Types

-- | Fork a task to run a discover function, forking a task with the provided
-- continuation applied to each discovered project
withDiscoveredProjects ::
  ( Has AtomicCounter sig m
  , Has (Lift IO) sig m
  , Has Debug sig m
  , Has TaskPool sig m
  , Has Logger sig m
  , Has Stack sig m
  ) =>
  -- | Discover function
  (Path Abs Dir -> StickyLogStackC (DiagDebugC (Diag.DiagnosticsC m)) [DiscoveredProject run]) ->
  -- | whether to unpack archives
  Path Abs Dir ->
  (DiscoveredProject run -> m ()) ->
  m ()
withDiscoveredProjects discover basedir f = forkTask $ do
  projectsResult <- Diag.runDiagnosticsIO . diagToDebug . stickyLogStack . withEmptyStack $ discover basedir
  Diag.withResult SevError SevWarn projectsResult (traverse_ (forkTask . f))
