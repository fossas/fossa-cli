{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Monorepo (
  monorepoScan,
  toPathFilters,
  PathFilters (..),
) where

import App.Fossa.Config.Analyze (MonorepoAnalyzeConfig (..))
import App.Fossa.EmbeddedBinary (BinaryPaths, withWigginsBinary)
import App.Fossa.VPS.Scan.RunWiggins (
  PathFilters (..),
  WigginsOpts,
  execWiggins,
  generateWigginsMonorepoOpts,
  toPathFilters,
 )
import App.Types (BaseDir (unBaseDir))
import Control.Effect.Diagnostics (
  Diagnostics,
  context,
 )
import Control.Effect.Lift (Lift)
import Effect.Exec (Exec)
import Effect.Logger (Has, Logger, logInfo)

monorepoScan ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has Exec sig m
  ) =>
  MonorepoAnalyzeConfig ->
  m ()
monorepoScan MonorepoAnalyzeConfig{..} = withWigginsBinary $ \binaryPaths -> do
  let wigginsOpts =
        generateWigginsMonorepoOpts
          (unBaseDir monorepoBasedir)
          monorepoAnalyzeOpts
          (toPathFilters monorepoFilters)
          monorepoSeverity
          monorepoRevision
          monorepoApiOpts
          monorepoMetadata

  logInfo "Running monorepo scan"
  context "Monorepo" $ runWiggins binaryPaths wigginsOpts

runWiggins :: (Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m ()
runWiggins binaryPaths opts = do
  context "Running monorepo binary" $ execWiggins binaryPaths opts
