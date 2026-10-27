-- | Per-visitor drivers for running strategies over the shared directory walk
-- from "Discovery.WalkStream".
--
-- The old per-strategy discovery ran one walk per strategy whose callback
-- returned a 'WalkStep'.  With a single shared listing stream, each visitor
-- must keep its own participation state: a visitor's own
-- 'Discovery.Walk.WalkSkipAll', 'Discovery.Walk.WalkSkipSome' or
-- 'Discovery.Walk.WalkStop' decision prunes only that visitor.  The walk
-- itself descends into every directory, so one strategy's pruning can never
-- starve another (see "Discovery.Walk" for the per-strategy semantics these
-- drivers mirror).
module Discovery.WalkVisitor (
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

import Data.Maybe (mapMaybe)
import Data.String.Conversion (toString)
import Data.Traversable qualified as Trav
import Discovery.Walk (WalkStep (..))
import Discovery.WalkStream (DirListing (dlDir, dlFiles, dlSubdirs))
import Path (Abs, Dir, File, Path, isProperPrefixOf, parseRelDir, (</>))
import Streaming (Of (..), Stream, lift)
import Streaming.Prelude qualified as S

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
  }
  deriving (Eq, Show)

-- | State of a fresh visitor.
initialVisitorState :: VisitorState
initialVisitorState = VisitorState{vsDone = False, vsPruned = []}

-- | Whether @dir@ is at or below a subtree root of a live skip decision.
-- Stale decisions (whose scope the DFS has backtracked above) are ignored;
-- 'applyVisitorStep' additionally drops them from the state.
isSwallowed :: VisitorState -> Path Abs Dir -> Bool
isSwallowed state dir =
  any
    (\root -> root == dir || isProperPrefixOf root dir)
    [root | (scope, roots) <- vsPruned state, atOrAboveScope dir scope, root <- roots]
  where
    atOrAboveScope candidate scope = scope == candidate || isProperPrefixOf scope candidate

-- | Fold a visitor's 'WalkStep' into its own state, mirroring the
-- per-strategy walk in "Discovery.Walk": the step was produced at @dir@ (so
-- the visit still counts), and only that visitor is affected.
applyVisitorStep :: WalkStep -> Path Abs Dir -> VisitorState -> VisitorState
applyVisitorStep step dir state0 =
  let state = state0{vsPruned = pruneDead (vsPruned state0)}
   in case step of
        WalkContinue -> state
        -- The visitor stopped: keep the live decisions for determinism, but
        -- they no longer matter because the visitor is never invoked again.
        WalkStop -> state{vsDone = True}
        WalkSkipAll -> state{vsPruned = (dir, [dir]) : vsPruned state}
        WalkSkipSome names -> state{vsPruned = (dir, roots) : vsPruned state}
          where
            roots = mapMaybe (fmap (dir </>) . parse) names
            -- Unparseable names are dropped silently, exactly like
            -- 'Discovery.Walk.walk'.
            parse name = parseRelDir (toString name)
  where
    -- Decisions whose scope the DFS has backtracked above are dead forever.
    pruneDead = filter (\(scope, _) -> scope == dir || isProperPrefixOf scope dir)

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
              let state' = applyVisitorStep step (dlDir dl) state
              S.yield (dlDir dl, o) <> loop state' rest

-- | Drive a visitor from an effectful next-listing source (e.g. a channel
-- read) rather than a walk stream; visitor outputs are discarded — callers
-- collect results through their own effects.  See the GHC-39999 note in
-- "Discovery.WalkBroadcast" for why this takes an action instead of a
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
              loop (applyVisitorStep step (dlDir dl) state)

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
          pure (Just o, applyVisitorStep step dir state)

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
