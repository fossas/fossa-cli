{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.Scan (
  scanMain,
) where

import App.Fossa.EmbeddedBinary (BinaryPaths, withWigginsBinary)
import App.Fossa.VPS.Scan.RunWiggins (
  ScanType (ScanType),
  WigginsOpts,
  execWiggins,
  generateWigginsScanOpts,
 )
import App.NewFossa.Config.VPS (
  AnalyzeConfig (..),
  FollowSymlinks (..),
  LicenseOnlyScan (..),
  SkipIPRScan (..),
 )
import App.Types (BaseDir (..))
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Data.Flag (fromFlag)
import Data.Text (Text)
import Effect.Exec (Exec)
import Effect.Logger (Has, Logger, Pretty (pretty), logInfo)

scanMain ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has Logger sig m
  ) =>
  AnalyzeConfig ->
  m ()
scanMain AnalyzeConfig{..} = withWigginsBinary $ \binaryPaths -> do
  let scanType =
        ScanType
          (fromFlag FollowSymlinks followSymlinks)
          (fromFlag SkipIPRScan skipIPRScan)
          (fromFlag LicenseOnlyScan licenseOnlyScan)
  let wigginsOpts =
        generateWigginsScanOpts
          (unBaseDir analyzeBaseDir)
          analyzeSeverity
          analyzeRevision
          scanType
          fileFilters
          analyzeApiOpts
          analyzeMetadata

  logInfo "Running VPS plugin scan"
  stdout <- runWiggins binaryPaths wigginsOpts
  logInfo $ pretty stdout

runWiggins :: (Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m Text
runWiggins binaryPaths opts = do
  execWiggins binaryPaths opts
