{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Snippets.Commit (
  commitWithMillhone,
) where

import App.Fossa.Config.Snippets (CommitConfig (..), labelForKind, labelForTarget, labelForTransform)
import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withMillhoneBinary)
import App.Types (unBaseDir)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Data.String.Conversion (toText)
import Effect.Exec (AllowErr (Never), Command (..), Exec, argFromPath, argsLabeled, execEffectful)
import Effect.Logger (Logger)
import Path (Abs, Dir, Path)

commitWithMillhone ::
  ( Has (Lift IO) sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  CommitConfig ->
  m ()
commitWithMillhone conf = withMillhoneBinary $ \bin -> execEffectful root $ mkCmd bin root conf
  where
    root = unBaseDir $ commitScanDir conf

mkCmd :: BinaryPaths -> Path Abs Dir -> CommitConfig -> Command
mkCmd bin root CommitConfig{..} =
  Command
    { cmdName = toText $ toPath bin
    , cmdArgs = concat [debug, cmd, output, format, overwriteOutput, targets, kinds, transforms, dir]
    , cmdAllowErr = Never
    }
  where
    cmd = ["commit"]
    dir = [argFromPath root]
    debug = if commitDebug then ["--log-level", "debug", "--log-format", "json"] else []
    targets = if null commitTargets then [] else argsLabeled labelForTarget commitTargets
    kinds = if null commitKinds then [] else argsLabeled labelForKind commitKinds
    transforms = if null commitTransforms then [] else argsLabeled labelForTransform commitTransforms
    output = ["--analyze-output-dir", argFromPath commitAnalyzeOutput]
    format = case commitOutputFormat of
      Just format' -> ["--format", toText format']
      Nothing -> []
    overwriteOutput = if commitOverwriteFossaDeps then ["--overwrite-fossa-deps"] else []
