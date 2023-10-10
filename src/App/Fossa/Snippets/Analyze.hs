{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Snippets.Analyze (
  analyzeWithMillhone,
) where

import App.Fossa.Config.Snippets (AnalyzeConfig (..), labelForKind, labelForTarget, labelForTransform)
import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withMillhoneBinary)
import App.Types (BaseDir (unBaseDir))
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Data.String.Conversion (toText)
import Effect.Exec (AllowErr (Never), Command (..), Exec, argFromPath, argsLabeled, execEffectful)
import Effect.Logger (Logger)
import Path (Abs, Dir, Path)

analyzeWithMillhone ::
  ( Has (Lift IO) sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  AnalyzeConfig ->
  m ()
analyzeWithMillhone conf = withMillhoneBinary $ \bin -> execEffectful root $ mkCmd bin root conf
  where
    root = unBaseDir $ analyzeScanDir conf

mkCmd :: BinaryPaths -> Path Abs Dir -> AnalyzeConfig -> Command
mkCmd bin root AnalyzeConfig{..} =
  Command
    { cmdName = toText $ toPath bin
    , cmdArgs = "analyze" : argFromPath root : concat [output, overwriteOutput, targets, kinds, transforms]
    , cmdAllowErr = Never
    }
  where
    targets = if null analyzeTargets then [] else argsLabeled labelForTarget analyzeTargets
    kinds = if null analyzeKinds then [] else argsLabeled labelForKind analyzeKinds
    transforms = if null analyzeTransforms then [] else argsLabeled labelForTransform analyzeTransforms
    output = ["--output", argFromPath analyzeOutput]
    overwriteOutput = if analyzeOverwriteOutput then ["--overwrite-output"] else []
