{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ListTargets (
  listTargetsMain,
  listSubCommand,
) where

import App.Fossa.Analyze.Discover (discoverFuncs)
import App.Fossa.Analyze.DiscoveryDriver (PreparedEntry (..), entryConsumers, prepare, runDiscovery)
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
import Control.Carrier.StickyLogger (StickyLogger, logSticky', runStickyLogger)
import Control.Carrier.TaskPool (
  Progress (..),
  TaskPool,
  withTaskPool,
 )
import Control.Concurrent (getNumCapabilities)
import Control.Effect.Debug (Debug)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Reader (ask)
import Control.Effect.Stack (Stack)
import Control.Effect.Telemetry (Telemetry)
import Data.Aeson (ToJSON, encode, object, (.=))
import Data.Aeson.Extra (encodeJSONToText)
import Data.Foldable (for_, traverse_)
import Data.Set.NonEmpty (toSet)
import Data.String.Conversion (decodeUtf8, toText)
import Discovery.Filters (AllFilters, MavenScopeFilters (..))
import Discovery.Projects (withDiscoveredProjects)
import Discovery.Walk.Broadcast (withWalkBroadcast)
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
import Types (BuildTarget (..), DiscoveredProject (..), FoundTargets (..))

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
runAll outputFmt basedir = do
  filters <- ask @AllFilters
  entriesResult <- runDiscovery $ do
    entries <- traverse prepare discoverFuncs
    -- 'False': list-targets runs without the default filters, and projects
    -- under non-production paths (node_modules, vendor, ...) are part of its
    -- output.
    withWalkBroadcast filters Nothing False basedir (concatMap entryConsumers entries)
    pure entries
  Diag.withResult SevError SevWarn entriesResult (traverse_ single)
  where
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
