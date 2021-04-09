{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ListTargets
  ( listTargetsMain,
  )
where

import App.Fossa.Analyze (discoverFuncs)
import App.Types (BaseDir (..))
import Control.Carrier.Finally
import Control.Carrier.TaskPool
import Control.Concurrent (getNumCapabilities)
import Data.Foldable (for_)
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Path (toFilePath)
import Path.IO (makeRelative)
import Types (BuildTarget (..), DiscoveredProject (..))
import qualified Control.Carrier.Diagnostics as Diag

type DummyM = ReadFSIOC (ExecIOC (Diag.DiagnosticsC IO))

listTargetsMain :: BaseDir -> IO ()
listTargetsMain (BaseDir basedir) = do
  capabilities <- getNumCapabilities

  withLogger SevInfo . runFinally . withTaskPool capabilities updateProgress . runReadFSIO . runExecIO $ do
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

          for_ (projectBuildTargets project) $ \target -> do
            logInfo $
              "Found target: "
                <> pretty (projectType project)
                <> "@"
                <> pretty (toFilePath rel)
                <> ":"
                <> pretty (unBuildTarget target)

updateProgress :: Has Logger sig m => Progress -> m ()
updateProgress Progress {..} =
  logSticky
    ( "[ "
        <> annotate (color Cyan) (pretty pQueued)
        <> " Waiting / "
        <> annotate (color Yellow) (pretty pRunning)
        <> " Running / "
        <> annotate (color Green) (pretty pCompleted)
        <> " Completed"
        <> " ]"
    )
