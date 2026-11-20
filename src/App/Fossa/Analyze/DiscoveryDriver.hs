module App.Fossa.Analyze.DiscoveryDriver (
  DiscoveryM,
  runDiscovery,
  PreparedEntry (..),
  prepare,
  entryConsumers,
) where

import App.Fossa.Analyze.Debug (DiagDebugC, diagToDebug)
import App.Fossa.Analyze.Discover (DiscoverFunc (..))
import App.Fossa.Analyze.Types (AnalyzeProject)
import Control.Carrier.AtomicCounter (AtomicCounter)
import Control.Carrier.Debug (Debug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Finally (Has)
import Control.Carrier.Stack.StickyLog (StickyLogStackC, stickyLogStack)
import Control.Effect.Diagnostics (fatalText)
import Control.Effect.Exception (Lift)
import Control.Effect.Lift (sendIO)
import Control.Effect.Stack (Stack, withEmptyStack)
import Data.Aeson qualified as Aeson
import Data.IORef (newIORef, readIORef)
import Diag.Result (Result)
import Discovery.Walk.Broadcast (WalkConsumer (..), accumulateConsumer)
import Effect.Logger (Logger)
import Path (Abs, Dir, Path)
import Types (DiscoveredProject (..), projectTypeToText)

-- | The monad strategy discovery runs in: the base effects wrapped in the
-- same carrier stack 'Discovery.Projects.withDiscoveredProjects' uses per
-- strategy fork.
type DiscoveryM m = StickyLogStackC (DiagDebugC (Diag.DiagnosticsC m))

-- | Run a 'DiscoveryM' computation to a 'Result': one Diagnostics scope around
-- a whole discovery phase, under the same carriers
-- 'Discovery.Projects.withDiscoveredProjects' uses per strategy fork, so a
-- strategy's failure renders the same way everywhere.
runDiscovery ::
  ( Has (Lift IO) sig m
  , Has AtomicCounter sig m
  , Has Debug sig m
  , Has Logger sig m
  , Has Stack sig m
  ) =>
  DiscoveryM m a -> m (Result a)
runDiscovery = Diag.runDiagnosticsIO . diagToDebug . stickyLogStack . withEmptyStack

-- | A 'DiscoverFunc' prepared for the shared discovery walk: its broadcast
-- consumer(s) (empty for strategies that still discover themselves) and a
-- discovery action that reads the strategy's collected projects.
data PreparedEntry d where
  PreparedEntry ::
    ( AnalyzeProject p
    , Aeson.ToJSON p
    ) =>
    [WalkConsumer () d] ->
    (Path Abs Dir -> d [DiscoveredProject p]) ->
    PreparedEntry d

-- | Prepare a 'DiscoverFunc' for the shared discovery walk: its broadcast
-- consumer(s) plus the discovery action that reads the strategy's collected
-- projects.  Shared by 'App.Fossa.Analyze.runAnalyzers', the container sources,
-- and @fossa list-targets@ so every caller mirrors the host analysis exactly.
--
-- No 'Has Diagnostics' clause: the phase monad's innermost layer provides it,
-- and @list-targets@' outer monad has none.  The four clauses are the outer
-- effects GHC needs to derive the phase monad's constraints.
prepare ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , Has Debug sig m
  , Has Stack sig m
  ) =>
  DiscoverFunc (DiscoveryM m) ->
  DiscoveryM m (PreparedEntry (DiscoveryM m))
prepare dscover = case dscover of
  DiscoverFunc f -> pure (PreparedEntry [] f)
  WalkedFunc projectType visitor finalize -> do
    sink <- sendIO (newIORef (Right ([] :: [raw])))
    let msg = "Error discovering projects for " <> projectTypeToText projectType
        -- Read the sink in the strategy finalizer: a failure recorded during
        -- the walk makes this throw (see 'accumulateConsumer'), so
        -- 'withDiscoveredProjects' renders it at ERROR severity and discards
        -- the whole strategy — the pre-broadcast all-or-nothing contract.
        collect = do
          state <- sendIO (readIORef sink)
          case state of
            Left e -> fatalText e
            Right projects -> pure projects
    pure
      (PreparedEntry [accumulateConsumer projectType msg sink visitor] $ \dir -> finalize dir collect)

-- | The broadcast consumer(s) a prepared entry contributes to the shared walk:
-- empty for strategies that still discover themselves, one 'accumulateConsumer'
-- for those driven by the walk.
entryConsumers :: PreparedEntry d -> [WalkConsumer () d]
entryConsumers (PreparedEntry cs _) = cs
