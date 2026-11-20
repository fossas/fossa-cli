{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ListTargets (
  listTargetsMain,
  listSubCommand,
) where

import App.Fossa.Analyze.Debug (DiagDebugC, diagToDebug)
import App.Fossa.Analyze.Discover (DiscoverFunc (..), discoverFuncs)
import App.Fossa.Analyze.Types (AnalyzeProject)
import App.Fossa.Config.Analyze (StrategyConfig)
import App.Fossa.Config.ListTargets (
  ListTargetOutputFormat (..),
  ListTargetsCliOpts,
  ListTargetsConfig (..),
  mkSubCommand,
 )
import App.Fossa.Subcommand (SubCommand)
import App.Types (BaseDir (..), Mode (..))
import Control.Carrier.AtomicCounter (
  AtomicCounter,
  Has,
  runAtomicCounter,
 )
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Finally (runFinally)
import Control.Carrier.Reader (Reader, runReader)
import Control.Carrier.Stack.StickyLog (StickyLogStackC, stickyLogStack)
import Control.Carrier.StickyLogger (StickyLogger, logSticky', runStickyLogger)
import Control.Carrier.TaskPool (
  Progress (..),
  TaskPool,
  withTaskPool,
 )
import Control.Concurrent (getNumCapabilities)
import Control.Effect.Debug (Debug)
import Control.Effect.Diagnostics (fatalText)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Reader (ask)
import Control.Effect.Stack (Stack, withEmptyStack)
import Control.Effect.Telemetry (Telemetry)
import Data.Aeson (ToJSON, encode, object, (.=))
import Data.Aeson.Extra (encodeJSONToText)
import Data.Foldable (for_, traverse_)
import Data.IORef (newIORef, readIORef)
import Data.Set.NonEmpty (toSet)
import Data.String.Conversion (decodeUtf8, toText)
import Diag.Result (Result)
import Discovery.Filters (AllFilters, MavenScopeFilters (..))
import Discovery.Projects (withDiscoveredProjects)
import Discovery.Walk.Broadcast (WalkConsumer (..), accumulateConsumer, withWalkBroadcast)
import Effect.Exec (Exec)
import Effect.Logger (
  Color (Cyan, Green, Yellow),
  Logger,
  Pretty (pretty),
  Severity (SevError, SevInfo, SevWarn),
  annotate,
  color,
  logDebug,
  logInfo,
  logStdout,
  logWarn,
 )
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, Path, toFilePath)
import Path.IO (makeRelative)
import Types (BuildTarget (..), DiscoveredProject (..), FoundTargets (..), projectTypeToText)

listSubCommand :: SubCommand ListTargetsCliOpts ListTargetsConfig
listSubCommand = mkSubCommand listTargetsMain

listTargetsMain ::
  ( Has Logger sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Stack sig m
  , Has Telemetry sig m
  ) =>
  ListTargetsConfig ->
  m ()
listTargetsMain ListTargetsConfig{..} = do
  capabilities <- sendIO getNumCapabilities
  logWarn "fossa list-targets does not apply any filtering, you may see projects which are not present in the final analysis."
  ignoreDebug
    . runStickyLogger SevInfo
    . runFinally
    . withTaskPool capabilities updateProgress
    . runAtomicCounter
    . runReader strategyConfig
    . runReader (MavenScopeIncludeFilters mempty)
    . runReader (NonStrict)
    -- list-targets has no filter support yet (TODO: discovery and post-discovery
    -- filtering).
    . runReader (mempty :: AllFilters)
    $ runAll listTargetOutputFormat (unBaseDir baseDir)

runAll ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has TaskPool sig m
  , Has (Lift IO) sig m
  , Has AtomicCounter sig m
  , Has Debug sig m
  , Has Stack sig m
  , Has (Reader StrategyConfig) sig m
  , Has (Reader MavenScopeFilters) sig m
  , Has (Reader Mode) sig m
  , Has (Reader AllFilters) sig m
  , Has Telemetry sig m
  ) =>
  ListTargetOutputFormat ->
  Path Abs Dir ->
  m ()

-- | Deliberately mirrors 'App.Fossa.Analyze''s 'DiscoveryM' — keep the two in sync.
type DiscoveryM m = StickyLogStackC (DiagDebugC (Diag.DiagnosticsC m))

-- | A strategy's failure here renders the same way it would under @fossa analyze@.
runDiscovery ::
  ( Has (Lift IO) sig m
  , Has AtomicCounter sig m
  , Has Debug sig m
  , Has Logger sig m
  , Has Stack sig m
  ) =>
  DiscoveryM m a ->
  m (Result a)
runDiscovery = Diag.runDiagnosticsIO . diagToDebug . stickyLogStack . withEmptyStack

-- | A 'DiscoverFunc' readied for the shared walk; consumers are empty for
-- strategies that still discover themselves.
data PreparedEntry d where
  PreparedEntry ::
    ( AnalyzeProject p
    , ToJSON p
    ) =>
    [WalkConsumer () d] ->
    (Path Abs Dir -> d [DiscoveredProject p]) ->
    PreparedEntry d

runAll outputFmt basedir = do
  filters <- ask @AllFilters
  entriesResult <- runDiscovery $ do
    entries <- traverse prepare discoverFuncs
    withWalkBroadcast filters basedir (concatMap entryConsumers entries)
    pure entries
  Diag.withResult SevError SevWarn entriesResult (traverse_ single)
  where
    -- No Diagnostics clause: the phase monad's innermost layer provides it.
    -- These clauses are the outer effects GHC needs to derive the constraints
    -- for 'discoverFuncs'; 'runAll' supplies all of them.
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
            -- Reading the sink in the finalizer turns a failure recorded during the
            -- walk into a throw (see 'accumulateConsumer'), so
            -- 'withDiscoveredProjects' discards the whole strategy — preserving
            -- list-targets' all-or-nothing contract.
            collect = do
              state <- sendIO (readIORef sink)
              case state of
                Left e -> fatalText e
                Right projects -> pure projects
        pure (PreparedEntry [accumulateConsumer projectType msg sink visitor] $ \dir -> finalize dir collect)

    entryConsumers (PreparedEntry cs _) = cs
    single (PreparedEntry _ discover) = withDiscoveredProjects discover basedir formatter
    formatter proj = case outputFmt of
      Legacy -> renderLegacyFmt basedir proj
      Text -> renderTextFmt basedir proj
      NdJSON -> renderNdJson basedir proj

-- | Render targets in ndjson format
-- Reference: https://github.com/ndjson/ndjson-spec
renderNdJson :: (Has Logger sig m, Has (Lift IO) sig m) => Path Abs Dir -> DiscoveredProject a -> m ()
renderNdJson basedir project = do
  let maybeRel = makeRelative basedir (projectPath project)
  let render txt = logStdout . decodeUtf8 $ (encode txt) <> "\n"
  case maybeRel of
    Nothing -> pure ()
    Just rel ->
      case projectBuildTargets project of
        ProjectWithoutTargets ->
          render $ object ["type" .= projectType project, "path" .= toFilePath rel]
        FoundTargets targets -> for_ (toSet targets) $ \target -> do
          render $ object ["type" .= projectType project, "path" .= toFilePath rel, "target" .= unBuildTarget target]

-- | Render targets in textual format.
-- Textual format is compatible with --only-target and --exclude-target argument
renderTextFmt :: (Has Logger sig m, Has (Lift IO) sig m) => Path Abs Dir -> DiscoveredProject a -> m ()
renderTextFmt basedir project = do
  let maybeRel = makeRelative basedir $ projectPath project
  let render txt = logStdout $ txt <> "\n"

  case maybeRel of
    Nothing -> pure ()
    Just rel ->
      case projectBuildTargets project of
        ProjectWithoutTargets -> do
          render $ toText (projectType project) <> "@" <> toText (toFilePath rel)
        FoundTargets targets -> for_ (toSet targets) $ \target -> do
          render $ toText (projectType project) <> "@" <> toText (toFilePath rel) <> ":" <> toText (unBuildTarget target)

renderLegacyFmt :: (ToJSON a, Has Logger sig m) => Path Abs Dir -> DiscoveredProject a -> m ()
renderLegacyFmt basedir project = do
  let maybeRel = makeRelative basedir (projectPath project)

  case maybeRel of
    Nothing -> pure ()
    Just rel -> do
      logInfo $
        "Found project: "
          <> pretty (projectType project)
          <> "@"
          <> pretty (toFilePath rel)

      logDebug . pretty . encodeJSONToText $ projectData project

      case projectBuildTargets project of
        ProjectWithoutTargets -> do
          logInfo $
            "Found target: "
              <> pretty (projectType project)
              <> "@"
              <> pretty (toFilePath rel)
        FoundTargets targets -> for_ (toSet targets) $ \target -> do
          logInfo $
            "Found target: "
              <> pretty (projectType project)
              <> "@"
              <> pretty (toFilePath rel)
              <> ":"
              <> pretty (unBuildTarget target)

updateProgress :: Has StickyLogger sig m => Progress -> m ()
updateProgress Progress{..} =
  logSticky'
    ( "[ "
        <> annotate (color Cyan) (pretty pQueued)
        <> " Waiting / "
        <> annotate (color Yellow) (pretty pRunning)
        <> " Running / "
        <> annotate (color Green) (pretty pCompleted)
        <> " Completed"
        <> " ]"
    )
