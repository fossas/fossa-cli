{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Snippets.GenerateFingerprints (
  generateFingerprints,
) where

import App.Fossa.Config.Snippets (GenerateFingerprintsConfig (..))
import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withScanossPyBinary)
import App.Types (unBaseDir)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Data.String.Conversion (toText)
import Effect.Exec (AllowErr (Never), Command (..), Exec, argFromPath, execEffectful)
import Effect.Logger (Logger, logInfo)
import Path (Abs, Dir, Path)

generateFingerprints ::
  ( Has (Lift IO) sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  GenerateFingerprintsConfig ->
  m ()
generateFingerprints conf = do
  logInfo "Generating WFP fingerprints for snippet scanning"
  withScanossPyBinary $ \bin -> execEffectful root $ mkCmd bin root conf
  where
    root = unBaseDir $ fingerprintsScanDir conf

mkCmd :: BinaryPaths -> Path Abs Dir -> GenerateFingerprintsConfig -> Command
mkCmd bin root GenerateFingerprintsConfig{..} =
  Command
    { cmdName = toText $ toPath bin
    , cmdArgs = concat [wfpCmd, debug, output, overwriteFlag, dir]
    , cmdAllowErr = Never
    }
  where
    wfpCmd = ["wfp"]
    dir = [argFromPath root]
    debug = if fingerprintsDebug then ["--debug"] else []
    output = ["-o", toText fingerprintsOutput]
    overwriteFlag = if fingerprintsOverwrite then ["--overwrite"] else [] 
