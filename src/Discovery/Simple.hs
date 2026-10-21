module Discovery.Simple (cachedDiscover, cachedDiscoverWith, simpleDiscover) where

import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Effect.Reader (Reader)
import Data.String.Conversion (toText)
import Discovery.Filters (AllFilters, withToolFilter)
import Effect.ReadFS (Has, ReadFS)
import Path (Abs, Dir, Path)
import Types (DiscoveredProject, DiscoveredProjectType)

-- | @simpleDiscover findProjects mkProject FooProjectType@ removes the
-- boilerplate of the /overwhelmingly/ common simple form of the @discover@
-- function.
--
-- For a discovery function to be simple, it must be expressable as
-- @map mkProject <$> findProjects (dir :: Path Abs Dir)@.  We add some
-- diagnostics and filtering, but essentially run that as-is.
--
-- The type of the @findProjects@ and @mkProject@ fields enforce that only
-- those discovery functions which actually conform to the simple discovery
-- model.  In particular, the @a@ in @m [a]@ returned by @findProjects@ must be
-- the same as in the final @m [DiscoveredProject a]@, and the type of
-- @mkProject@ enforces that.
--
-- If you can define a discovery function in terms of @simpleDiscover@, it's
-- most likely safe to do so.
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
simpleDiscover findProjects mkProject tool dir = withToolFilter tool $
  context (toText tool) $ do
    projects <- context "Finding Projects" $ findProjects dir
    pure $ map mkProject projects

-- | Like 'simpleDiscover', but for strategies whose raw projects were already
-- collected by a shared filesystem walk (see "Discovery.WalkStream") instead
-- of by the strategy walking the tree itself.  Applies the same tool filter
-- and diagnostics contexts as 'simpleDiscover'.
cachedDiscover ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  -- | Passed to 'withToolFilter'
  DiscoveredProjectType ->
  -- | @mkProject@
  (a -> DiscoveredProject a) ->
  -- | raw projects collected by the shared walk
  m [a] ->
  m [DiscoveredProject a]
cachedDiscover tool mkProject collect = withToolFilter tool $
  context (toText tool) $ do
    projects <- context "Finding Projects" collect
    pure $ map mkProject projects

-- | Like 'cachedDiscover', but for strategies whose collected raw projects
-- need a custom conversion step (e.g. one walk-directory entry producing many
-- projects).
cachedDiscoverWith ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  -- | Passed to 'withToolFilter'
  DiscoveredProjectType ->
  -- | turn the collected raw projects into discovered projects
  ([raw] -> m [DiscoveredProject p]) ->
  -- | raw projects collected by the shared walk
  m [raw] ->
  m [DiscoveredProject p]
cachedDiscoverWith tool post collect =
  withToolFilter tool $
    context (toText tool) $
      context "Finding Projects" (post =<< collect)
