module Discovery.Simple (simpleDiscover) where

import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Effect.Reader (Reader)
import Data.String.Conversion (toText)
import Discovery.Filters (AllFilters, withToolFilter)
import Effect.ReadFS (Has, ReadFS)
import Path (Abs, Dir, Path)
import Types (DiscoveredProject, DiscoveredProjectType)

simpleDiscover ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  -- | @findProjects@
  (Path Abs Dir -> m [a]) ->
  -- | @mkProject@
  (a -> DiscoveredProject a) ->
  -- | Passed to 'withToolFilter'
  DiscoveredProjectType ->
  -- | directory to start discovery in
  Path Abs Dir ->
  m [DiscoveredProject a]
simpleDiscover finder toDiscProj tool dir = withToolFilter tool $
  context (toText tool) $ do
    projects <- context "Finding Projects" $ finder dir
    pure $ map toDiscProj projects
