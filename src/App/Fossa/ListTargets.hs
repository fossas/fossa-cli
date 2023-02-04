{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ListTargets (
  listTargetsMain,
  listSubCommand,
) where

import App.Fossa.Analyze.Discover (DiscoverFunc (DiscoverFunc), discoverFuncs)
import App.Fossa.Config.Analyze (ExperimentalAnalyzeConfig)
import App.Fossa.Config.ListTargets (
  ListTargetsCliOpts,
  ListTargetsConfig (..),
  mkSubCommand,
 )
import App.Fossa.Subcommand (SubCommand)
import App.Types (BaseDir (..), OverrideDynamicAnalysisBinary)
import Control.Carrier.AtomicCounter (
  AtomicCounter,
  Has,
  runAtomicCounter,
 )
import Control.Carrier.Debug (ignoreDebug)
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
import Control.Effect.Stack (Stack)
import Control.Effect.Telemetry (Telemetry)
import Data.Aeson (ToJSON)
import Data.Aeson.Extra (encodeJSONToText)
import Data.Foldable (for_, traverse_)
import Data.Set qualified as Set
import Data.Set.NonEmpty (toSet)
import Discovery.Filters (AllFilters)
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec (Exec)
import Effect.Logger (
  Color (Cyan, Green, Yellow),
  Logger,
  Pretty (pretty),
  Severity (SevInfo),
  annotate,
  color,
  logDebug,
  logInfo,
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
    . runReader experimental
    . runReader overrideDynamicAnalysis
    -- The current version of `fossa list-targets` does not support filters.
    -- TODO: support both discovery and post-discovery filtering.
    . runReader (mempty :: AllFilters)
    $ runAll (unBaseDir baseDir)

runAll ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has TaskPool sig m
  , Has (Lift IO) sig m
  , Has AtomicCounter sig m
  , Has Debug sig m
  , Has Stack sig m
  , Has (Reader ExperimentalAnalyzeConfig) sig m
  , Has (Reader AllFilters) sig m
  , Has (Reader OverrideDynamicAnalysisBinary) sig m
  , Has Telemetry sig m
  ) =>
  Path Abs Dir ->
  m ()
runAll basedir = traverse_ single discoverFuncs
  where
    single (DiscoverFunc f) = withDiscoveredProjects f basedir (printSingle basedir)

printSingle :: (ToJSON a, Has Logger sig m) => Path Abs Dir -> DiscoveredProject a -> m ()
printSingle basedir project = do
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
        FoundTargets targets -> for_ (Set.toList $ toSet targets) $ \target -> do
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
