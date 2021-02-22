module Discovery.Projects
  ( withDiscoveredProjects,
  )
where

import qualified Control.Carrier.Diagnostics as Diag
import Control.Effect.Finally
import Control.Effect.Lift
import Control.Effect.TaskPool
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (for_, traverse_)
import qualified Discovery.Archive as Archive
import Effect.Logger
import Effect.ReadFS (ReadFS)
import Path
import Types (DiscoveredProject)

-- | Run a list of discover functions in parallel, running the provided function
-- on each discovered project. Note that the provided function is also run in
-- parallel.
withDiscoveredProjects ::
  (Has ReadFS sig m, Has (Lift IO) sig m, MonadIO m, Has TaskPool sig m, Has Logger sig m, Has Finally sig m) =>
  -- | Discover functions
  [Path Abs Dir -> Diag.DiagnosticsC m [DiscoveredProject run]] ->
  -- | whether to unpack archives
  Bool ->
  Path Abs Dir ->
  (DiscoveredProject run -> m ()) ->
  m ()
withDiscoveredProjects discoverFuncs unpackArchives basedir f = do
  for_ discoverFuncs $ \discover -> forkTask $ do
    projectsResult <- Diag.runDiagnosticsIO (discover basedir)
    Diag.withResult SevError projectsResult (traverse_ (forkTask . f))

  when unpackArchives $ do
    res <- Diag.runDiagnosticsIO $ Archive.discover (\dir -> liftFoo $ withDiscoveredProjects discoverFuncs unpackArchives dir f) basedir
    Diag.withResult SevError res (const (pure ()))


liftFoo :: m a -> Diag.DiagnosticsC m a
liftFoo = undefined
