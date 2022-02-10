module App.Fossa.Config.DumpBinaries (
  mkSubCommand,
  DumpBinsConfig (..),
  DumpBinsOpts,
) where

import App.Fossa.Config.Common (baseDirArg, validateDir)
import App.Fossa.Subcommand (EffStack, GetSeverity, SubCommand (SubCommand))
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift)
import Effect.ReadFS (ReadFS)
import Options.Applicative (InfoMod, progDesc)
import Path (Abs, Dir, Path)

dumpInfo :: InfoMod a
dumpInfo = progDesc "Output all embedded binaries to specified path"

mkSubCommand :: (DumpBinsConfig -> EffStack ()) -> SubCommand DumpBinsOpts DumpBinsConfig
mkSubCommand = SubCommand "dump-binaries" dumpInfo cliParser noLoadConfig mergeOpts
  where
    cliParser = DumpBinsOpts <$> baseDirArg
    noLoadConfig = const $ pure Nothing

newtype DumpBinsOpts = DumpBinsOpts FilePath

instance GetSeverity DumpBinsOpts

newtype DumpBinsConfig = DumpBinsConfig (Path Abs Dir) deriving (Show)

mergeOpts ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  a ->
  b ->
  DumpBinsOpts ->
  m DumpBinsConfig
mergeOpts _ _ (DumpBinsOpts path) = DumpBinsConfig <$> validateDir path
