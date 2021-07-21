{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ListTargets (
  listTargetsMain,
) where

import App.Fossa.Analyze (discoverFuncs)
import App.Types (BaseDir (..))
import Control.Carrier.AtomicCounter
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Finally
import Control.Carrier.StickyLogger (StickyLogger, logSticky', runStickyLogger)
import Control.Carrier.TaskPool
import Control.Concurrent (getNumCapabilities)
import Data.Foldable (for_)
import Data.Set qualified as S
import Data.Set.NonEmpty
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Path (toFilePath)
import Path.IO (makeRelative)
import Types (BuildTarget (..), DiscoveredProject (..), FoundTargets (..))

type DummyM = ReadFSIOC (ExecIOC (Diag.DiagnosticsC (LoggerC IO)))

listTargetsMain :: BaseDir -> IO ()
listTargetsMain (BaseDir basedir) = do
  capabilities <- getNumCapabilities

  withDefaultLogger SevInfo
    . runStickyLogger SevInfo
    . runFinally
    . withTaskPool capabilities updateProgress
    . runReadFSIO
    . runExecIO
    . runAtomicCounter
    $ do
      withDiscoveredProjects discoverFuncs False basedir $ \(project :: DiscoveredProject DummyM) -> do
        let maybeRel = makeRelative basedir (projectPath project)

        case maybeRel of
          Nothing -> pure ()
          Just rel -> do
            logInfo $
              "Found project: "
                <> pretty (projectType project)
                <> "@"
                <> pretty (toFilePath rel)

            case projectBuildTargets project of
              ProjectWithoutTargets -> do
                logInfo $
                  "Found target: "
                    <> pretty (projectType project)
                    <> "@"
                    <> pretty (toFilePath rel)
              FoundTargets targets -> for_ (S.toList $ toSet targets) $ \target -> do
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
