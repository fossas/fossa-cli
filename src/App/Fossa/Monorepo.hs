{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Monorepo (
  monorepoMain,
) where

import App.Fossa.EmbeddedBinary
import App.Fossa.ProjectInference
import App.Fossa.VPS.Scan.RunWiggins
import App.Types
import Control.Carrier.Diagnostics
import Control.Effect.Lift (Lift)
import Data.Text
import Effect.Exec
import Effect.Logger
import Fossa.API.Types

monorepoMain :: BaseDir -> MonorepoAnalysisOpts -> Severity -> ApiOpts -> ProjectMetadata -> OverrideProject -> IO ()
monorepoMain basedir monoRepoAnalysisOpts logSeverity apiOpts projectMeta overrideProject = withDefaultLogger logSeverity $ do
  logWithExit_ $ withWigginsBinary $ monorepoScan basedir monoRepoAnalysisOpts logSeverity apiOpts projectMeta overrideProject

monorepoScan :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m) => BaseDir -> MonorepoAnalysisOpts -> Severity -> ApiOpts -> ProjectMetadata -> OverrideProject -> BinaryPaths -> m ()
monorepoScan (BaseDir basedir) monorepoAnalysisOpts logSeverity apiOpts projectMeta projectOverride binaryPaths = do
  projectRevision <- mergeOverride projectOverride <$> (inferProjectFromVCS basedir <||> inferProjectDefault basedir)
  saveRevision projectRevision

  let wigginsOpts = generateWigginsMonorepoOpts basedir monorepoAnalysisOpts logSeverity projectRevision apiOpts projectMeta

  logInfo "Running monorepo scan"
  stdout <- context "Monorepo" $ runExecIO $ runWiggins binaryPaths wigginsOpts
  logInfo $ pretty stdout

runWiggins :: (Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m Text
runWiggins binaryPaths opts = do
  context "Running monorepo binary" $ execWiggins binaryPaths opts
