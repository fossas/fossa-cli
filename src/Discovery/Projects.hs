{-# LANGUAGE UndecidableInstances #-}

module Discovery.Projects (
  withDiscoveredProjects,
) where

import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Diagnostics.StickyContext
import Control.Effect.AtomicCounter (AtomicCounter)
import Control.Effect.Finally
import Control.Effect.Lift
import Control.Effect.TaskPool
import Control.Monad (when)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Foldable (for_, traverse_)
import Discovery.Archive qualified as Archive
import Effect.Logger
import Effect.ReadFS (ReadFS)
import Path
import Types (DiscoveredProject)

-- | Run a list of discover functions in parallel, running the provided function
-- on each discovered project. Note that the provided function is also run in
-- parallel.
withDiscoveredProjects ::
  (Has AtomicCounter sig m, Has ReadFS sig m, Has (Lift IO) sig m, Has TaskPool sig m, Has Logger sig m, Has Finally sig m) =>
  -- | Discover functions
  [Path Abs Dir -> StickyDiagC (Diag.DiagnosticsC m) [DiscoveredProject run]] ->
  -- | whether to unpack archives
  Bool ->
  Path Abs Dir ->
  (DiscoveredProject run -> m ()) ->
  m ()
withDiscoveredProjects discoverFuncs unpackArchives basedir f = do
  for_ discoverFuncs $ \discover -> forkTask $ do
    projectsResult <- Diag.runDiagnosticsIO . stickyDiag $ discover basedir
    Diag.withResult SevError projectsResult (traverse_ (forkTask . f))

  when unpackArchives $ do
    res <- Diag.runDiagnosticsIO $ Archive.discover (\dir -> lift $ withDiscoveredProjects discoverFuncs unpackArchives dir f) basedir
    Diag.withResult SevError res (const (pure ()))
