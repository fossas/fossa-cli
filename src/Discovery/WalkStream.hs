-- | A streaming re-implementation of the directory walk in
-- "Discovery.Walk".  Instead of returning a strict list of every directory
-- visited, 'walkStream' is a 'Streaming.Prelude' producer that emits one
-- 'DirListing' per directory as the traversal reaches it.
--
-- The traversal order and pruning semantics are identical to 'walk' (with a
-- consumer that always 'Discovery.Walk.WalkContinue's) combined with the
-- global filter prune of 'walkWithFilters'':
--
--   * loop avoidance via directory identifiers ('Effect.ReadFS.getIdentifier'),
--   * a directory whose identifier cannot be obtained is skipped with a warning,
--   * a directory that cannot be listed is skipped with a warning,
--   * directories that fail 'Discovery.Filters.pathAllowed' (relative to the
--     root) are never descended into, and the root itself is always visited.
--
-- There is no per-consumer control: 'Discovery.Walk.WalkStop' never occurs, and
-- pruning is purely filter-based, applied once at the root.
module Discovery.WalkStream (
  -- * Directory listings
  DirListing (..),

  -- * Walking the filetree as a producer
  walkStream,
  walkStreamUnfiltered,
  walkStreamWithCollectors,

  -- * Merging per-strategy 'WalkStep' decisions
  mergeWalkSteps,
  applyWalkStep,
) where

import Control.Effect.Diagnostics (Diagnostics, recover, warn)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (toString, toText)
import Data.Traversable qualified as Trav
import Discovery.Filters (AllFilters, pathAllowed)
import Discovery.Walk (WalkStep (..))
import Effect.ReadFS
import Path
import Streaming (Of (..), Stream, lift, (<>))
import Streaming.Prelude qualified as S

-- | A single directory visit: the directory itself together with the raw
-- sub-directories and files reported by the filesystem for that directory.
data DirListing = DirListing
  { dlDir :: Path Abs Dir
  , dlSubdirs :: [Path Abs Dir]
  , dlFiles :: [Path Abs File]
  }
  deriving (Eq, Ord, Show)

-- | Walk the filetree rooted at @root@, emitting a 'DirListing' for every
-- directory the traversal visits, subject to the global 'AllFilters' prune.
walkStream ::
  (Has ReadFS sig m, Has Diagnostics sig m) =>
  AllFilters ->
  Path Abs Dir ->
  Stream (Of DirListing) m ()
walkStream filters root = go Set.empty root
  where
    -- \| Visit @curdir@ (if it has not been seen and its identifier resolves),
    -- emit its listing, then recurse into the sub-directories permitted by the
    -- filters.  Threads the set of already-visited identifiers for loop
    -- avoidance, mirroring 'Discovery.Walk.walkAvoidLoop'.
    go traversed curdir = do
      mId <- lift (recover (getIdentifier curdir))
      case mId of
        Nothing -> do
          lift (warn $ "unable to get identifier for: " <> toText (toFilePath curdir))
          S.each []
        Just dirId ->
          if Set.member dirId traversed
            then S.each []
            else walktree (Set.insert dirId traversed) curdir

    -- \| Emit the listing for @curdir@ and recurse into the sub-directories that
    -- are permitted by the filters (mirroring the 'Discovery.Walk.WalkContinue'
    -- case of 'walkWithFilters'': disallowed sub-directories are pruned).
    walktree traversed' curdir = do
      listing <- lift (recover (listDir curdir))
      case listing of
        Nothing -> do
          lift (warn $ "skipping remaining files in: " <> toText (toFilePath curdir))
          S.each []
        Just (subdirs, files) -> do
          let dl = DirListing curdir subdirs files
              allowedSubdirs =
                filter
                  ( \sub -> case stripProperPrefix root sub of
                      Just rel -> pathAllowed filters rel
                      Nothing -> True
                  )
                  subdirs
          -- Yield this directory, then recurse into the allowed sub-directories
          -- in order, lazily glued together with '<>'.  A foldr (rather than
          -- 'concat') keeps the element type and the result type ('()') distinct.
          S.yield dl
            <> foldr (\sub acc -> (go traversed' sub) <> acc) (S.each []) allowedSubdirs

-- | Like 'walkStream', but without applying any path filters.  Equivalent to
-- 'walkStream' with 'mempty' filters, which allows every directory.
walkStreamUnfiltered :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> Stream (Of DirListing) m ()
walkStreamUnfiltered = walkStream mempty

-- | Fold the 'WalkStep' decisions of every strategy collector that visited a
-- single directory into one.  A directory is descended into only when no
-- collector wants to skip one of its sub-directories, which reproduces the
-- union of the per-strategy 'Discovery.Walk.walkWithFilters'' skip decisions
-- the old one-walk-per-strategy discovery made.
mergeWalkSteps :: [WalkStep] -> WalkStep
mergeWalkSteps steps
  | WalkStop `elem` steps = WalkStop
  | WalkSkipAll `elem` steps = WalkSkipAll
  | otherwise = case [xs | WalkSkipSome xs <- steps] of
      [] -> WalkContinue
      xss -> WalkSkipSome (concat xss)

-- | Apply a merged 'WalkStep' to the sub-directories that have already passed
-- the global path-filter prune, returning the ones the walk should descend
-- into.  Mirrors the 'WalkSkipSome' handling in 'Discovery.Walk.walk'.
applyWalkStep :: WalkStep -> [Path Abs Dir] -> [Path Abs Dir]
applyWalkStep WalkContinue ds = ds
applyWalkStep WalkStop _ = []
applyWalkStep WalkSkipAll _ = []
applyWalkStep (WalkSkipSome names) ds =
  let parsed = mapMaybe (parseRelDir . toString) names
   in filter (\d -> dirname d `notElem` parsed) ds

-- | Like 'walkStream', but at every visited directory it feeds the listing to
-- each of the supplied per-directory collectors (in order), merges their
-- 'WalkStep' decisions, and descends only into the sub-directories the merged
-- decision keeps.  The collectors carry the side effects (accumulating matches,
-- shelling out, etc.); this producer only drives the shared traversal and
-- prunes it with the same global path filters as 'walkStream'.
--
-- This is what lets discovery run a single filesystem walk that serves every
-- strategy at once instead of one walk per strategy.
walkStreamWithCollectors ::
  (Has ReadFS sig m, Has Diagnostics sig m) =>
  AllFilters ->
  Path Abs Dir ->
  [DirListing -> m WalkStep] ->
  Stream (Of DirListing) m ()
walkStreamWithCollectors filters root collectors = go Set.empty root
  where
    go traversed curdir = do
      mId <- lift (recover (getIdentifier curdir))
      case mId of
        Nothing -> do
          lift (warn $ "unable to get identifier for: " <> toText (toFilePath curdir))
          S.each []
        Just dirId ->
          if Set.member dirId traversed
            then S.each []
            else walktree (Set.insert dirId traversed) curdir

    walktree traversed' curdir = do
      listing <- lift (recover (listDir curdir))
      case listing of
        Nothing -> do
          lift (warn $ "skipping remaining files in: " <> toText (toFilePath curdir))
          S.each []
        Just (subdirs, files) -> do
          let dl = DirListing curdir subdirs files
          steps <- lift (Trav.traverse (\c -> c dl) collectors)
          let mergedStep = mergeWalkSteps steps
              allowedSubdirs =
                filter
                  ( \sub -> case stripProperPrefix root sub of
                      Just rel -> pathAllowed filters rel
                      Nothing -> True
                  )
                  subdirs
          let toVisit = applyWalkStep mergedStep allowedSubdirs
          S.yield dl
            <> foldr (\sub acc -> (go traversed' sub) <> acc) (S.each []) toVisit
