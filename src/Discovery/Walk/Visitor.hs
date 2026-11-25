-- | Per-visitor drivers for running strategies over the shared directory walk
-- from "Discovery.Walk.Stream".
--
-- The old per-strategy discovery ran one walk per strategy whose callback
-- returned a 'WalkStep'.  With a single shared listing stream, each visitor
-- must keep its own participation state: a visitor's own
-- 'Discovery.Walk.WalkSkipAll', 'Discovery.Walk.WalkSkipSome' or
-- 'Discovery.Walk.WalkStop' decision prunes only that visitor.  The walk
-- itself descends into every directory, so one strategy's pruning can never
-- starve another (see "Discovery.Walk" for the per-strategy semantics these
-- drivers mirror).
module Discovery.Walk.Visitor (
  -- * Per-visitor state
  VisitorState (..),
  initialVisitorState,
  isSwallowed,
  applyVisitorStep,

  -- * Running visitors over a shared walk
  Visitor,
  runVisitor,
  runVisitors,
  driveVisitor,
) where

import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Traversable qualified as Trav
import Discovery.Walk (WalkStep (..))
import Discovery.Walk.Stream (DirListing (dlDir, dlFiles, dlSubdirs))
import Path (Abs, Dir, File, Path, dirname, parseRelDir, toFilePath)
import Streaming (Of (..), Stream, lift)
import Streaming.Prelude qualified as S
import System.FilePath qualified as FilePath

-- | Participation state of one visitor over the shared walk: whether the
-- visitor has stopped ('vsDone') and its live skip decisions ('vsPruned'),
-- where a swallowed listing is one whose directory equals or lies below one
-- of those decisions' subtree roots.
--
-- Each decision is a @(scope, roots)@ pair: @scope@ is the directory at
-- which the visitor emitted its 'Discovery.Walk.WalkSkipAll' or
-- 'Discovery.Walk.WalkSkipSome' step, @roots@ the subtree roots it produced
-- (one for 'Discovery.Walk.WalkSkipAll', k for 'Discovery.Walk.WalkSkipSome',
-- unparseable names dropped exactly like 'Discovery.Walk.walk').
--
-- Correctness relies on the shared stream being DFS-ordered: 'walkStream'
-- mirrors "Discovery.Walk"'s order, and 'driveVisitor' reads the same
-- listing sequence for every visitor.  A visitor is only invoked on
-- non-swallowed listings, so every decision it emits is made outside all
-- existing roots (roots stay pairwise disjoint) and a decision only matters
-- for the contiguous run of DFS listings inside its scope.  Once the walk
-- backtracks above a decision's scope, that entry is dead: 'applyVisitorStep'
-- drops dead entries at every non-swallowed listing, keeping the list
-- bounded by tree depth plus one decision's arity instead of growing with
-- every skip.
data VisitorState = VisitorState
  { vsDone :: Bool
  , vsPruned :: [(Path Abs Dir, [Path Abs Dir])]
  , vsRoot :: Maybe (Path Abs Dir)
  }
  deriving (Eq, Show)

-- | State of a fresh visitor.
initialVisitorState :: VisitorState
initialVisitorState = VisitorState{vsDone = False, vsPruned = [], vsRoot = Nothing}

-- | Whether @dir@ is at or below a subtree root of a live skip decision.
-- Stale decisions (whose scope the DFS has backtracked above) are ignored;
-- 'applyVisitorStep' additionally drops them from the state.
--
-- All comparisons go through 'dirKey': the roots and the listed directories
-- mix construction routes ('parseRelDir' normalizes to native separators on
-- Windows, the container tar tree keeps the archive's forward slashes), and
-- 'Path' equality compares internal strings, so the raw values would not
-- match across routes — the same mismatch #1861 fixed for the default-filter
-- predicate.
isSwallowed :: VisitorState -> Path Abs Dir -> Bool
isSwallowed state dir =
  any (\root -> key root == key dir || (key root `isPrefixOf` key dir)) liveRoots
  where
    liveRoots = concatMap snd (vsPruned state)
    key = dirKey

-- | The separator-normalized 'toFilePath' form of a directory, the key the
-- visitor's skip decisions compare on.  Roots always carry a trailing
-- separator ('ensureTrailingSeparator', 'parseRelDir'), so key-prefix
-- comparison is component-exact: @a/vendor/@ cannot prefix @a/vendorx/@.
dirKey :: Path b Dir -> FilePath
dirKey = map sep . toFilePath
  where
    sep c
      | FilePath.isPathSeparator c = '/'
      | otherwise = c

-- | Fold a visitor's 'WalkStep' into its own state, mirroring the
-- per-strategy walk in "Discovery.Walk": the step was produced at @dir@ (so
-- the visit still counts), and only that visitor is affected.
applyVisitorStep :: WalkStep -> Path Abs Dir -> [Path Abs Dir] -> VisitorState -> VisitorState
applyVisitorStep step dir subdirs state0 =
  let state =
        state0
          { vsRoot = case vsRoot state0 of Just root -> Just root; Nothing -> Just dir
          , vsPruned = pruneDead (vsPruned state0)
          }
   in case step of
        WalkContinue -> state
        -- The visitor stopped: keep the live decisions for determinism, but
        -- they no longer matter because the visitor is never invoked again.
        WalkStop -> state{vsDone = True}
        WalkSkipAll -> state{vsPruned = (dir, subdirs) : vsPruned state}
        WalkSkipSome names -> state{vsPruned = (dir, roots) : vsPruned state}
          where
            -- The roots are this listing's own subdirs whose directory name
            -- matches a skip name -- the same filter the per-strategy walk in
            -- "Discovery.Walk" applies, in the same path form later listings
            -- present.  Joining the names onto @dir@ instead breaks in the
            -- container walk: its root is the raw sentinel @Path "vfs-root"@
            -- ('IndexFileTree.fixedVfsRoot'), while 'IndexFileTree.lookupDir'
            -- strips that prefix from its listings, so a joined root like
            -- @vfs-root/node_modules/@ matches nothing and root-level skips
            -- (Node's node_modules/, Maven's target/) silently die.
            roots = filter (\sub -> Set.member (dirKey (dirname sub)) nameKeys) subdirs
            -- Unparseable names are dropped silently, exactly like
            -- 'Discovery.Walk.walk'.
            nameKeys =
              Set.fromList $
                mapMaybe
                  (fmap dirKey . parseRelDir . toString)
                  names
  where
    -- Decisions whose scope the DFS has backtracked above are dead forever --
    -- except the walk root's: its scope's form need not relate to the listings'
    -- (see the roots note above), and its subtree is the whole walk.
    pruneDead =
      filter
        (\(scope, _) -> matchesBelow scope dir || isRootScope scope)
    matchesBelow scope child = key scope == key child || (key scope `isPrefixOf` key child)
    isRootScope scope = maybe False (\root -> key scope == key root) (vsRoot state0)
    key = dirKey

-- | A strategy's per-directory collector, mirroring the old per-strategy walk
-- callback: it returns its output for the listing plus its next 'WalkStep'.
type Visitor o m = Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m (o, WalkStep)

-- | Run a single visitor over a shared walk stream, maintaining one
-- 'VisitorState' for it.
--
-- The underlying stream is drained fully: swallowing never stops the walk
-- itself, only this visitor's participation in later listings.  Yield policy:
-- exactly one @(dir, o)@ per listing where the visitor was actually invoked
-- (even when @o@ is empty); swallowed or post-'Discovery.Walk.WalkStop'
-- listings yield nothing.
runVisitor ::
  Monad m =>
  Visitor o m ->
  Stream (Of DirListing) m () ->
  Stream (Of (Path Abs Dir, o)) m ()
runVisitor visit = loop initialVisitorState
  where
    loop state input = do
      mnext <- lift (S.uncons input)
      case mnext of
        Nothing -> S.each []
        Just (dl, rest) ->
          if vsDone state || isSwallowed state (dlDir dl)
            then loop state rest
            else do
              (o, step) <- lift $ visit (dlDir dl) (dlSubdirs dl) (dlFiles dl)
              let state' = applyVisitorStep step (dlDir dl) (dlSubdirs dl) state
              S.yield (dlDir dl, o) <> loop state' rest

-- | Drive a visitor from an effectful next-listing source (e.g. a channel
-- read) rather than a walk stream; visitor outputs are discarded — callers
-- collect results through their own effects.  See the GHC-39999 note in
-- "Discovery.Walk.Broadcast" for why this takes an action instead of a
-- stream; the 'WalkStep' state threading mirrors 'runVisitor'.
driveVisitor :: Monad m => Visitor o m -> (m (Maybe DirListing)) -> m ()
driveVisitor visit next = loop initialVisitorState
  where
    loop state = do
      mnext <- next
      case mnext of
        Nothing -> pure ()
        Just dl
          | vsDone state || isSwallowed state (dlDir dl) -> loop state
          | otherwise -> do
              (_, step) <- visit (dlDir dl) (dlSubdirs dl) (dlFiles dl)
              loop (applyVisitorStep step (dlDir dl) (dlSubdirs dl) state)

-- | Run several visitors over one shared walk stream.  Each visitor keeps
-- its own 'VisitorState', so one visitor's 'WalkStep' decisions can only
-- swallow its own participation; the stream itself is drained fully either
-- way.  Yield policy: for every listing at which at least one visitor runs,
-- yield @(dir, oss)@ where @oss@ holds the outputs of the visitors that ran
-- at that listing, in list order; listings at which no visitor runs yield
-- nothing.
runVisitors ::
  Monad m =>
  [Visitor o m] ->
  Stream (Of DirListing) m () ->
  Stream (Of (Path Abs Dir, [o])) m ()
runVisitors visitors = loop states0
  where
    states0 = replicate (length visitors) initialVisitorState

    visitOne v state dir subdirs files =
      if vsDone state || isSwallowed state dir
        then pure (Nothing, state)
        else do
          (o, step) <- v dir subdirs files
          pure (Just o, applyVisitorStep step dir subdirs state)

    loop states input = do
      mnext <- lift (S.uncons input)
      case mnext of
        Nothing -> S.each []
        Just (dl, rest) -> do
          results <- lift $ Trav.sequence (zipWith (\v st -> visitOne v st (dlDir dl) (dlSubdirs dl) (dlFiles dl)) visitors states)
          let oss = mapMaybe fst results
              states' = map snd results
          if null oss
            then loop states' rest
            else S.yield (dlDir dl, oss) <> loop states' rest
