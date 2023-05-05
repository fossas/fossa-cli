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
  context, fatal,
 )
import Control.Effect.Lift (Lift)
import Data.Text (Text)
import Effect.Exec (Exec, runExecIO)
import Effect.Logger (Has, Logger, Pretty (pretty), logInfo)

monorepoScan ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
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

  case wigginsOpts of
    Left e -> fatal e
    Right wigginsOpts' -> do logInfo "Running monorepo scan"
                             stdout <- context "Monorepo" $ runExecIO $ runWiggins binaryPaths wigginsOpts'
                             logInfo $ pretty stdout

runWiggins :: (Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m Text
runWiggins binaryPaths opts = do
  context "Running monorepo binary" $ execWiggins binaryPaths opts


