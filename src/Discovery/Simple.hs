module Discovery.Simple (simpleDiscover) where

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
-- the same as the final @m [DiscoveredProject a]@, and the type of @mkProject@
-- enforces that.
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
