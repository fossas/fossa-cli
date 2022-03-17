module App.Fossa.DumpBinaries (
  dumpSubCommand,
) where

import App.Fossa.Config.DumpBinaries (
  DumpBinsConfig (..),
  DumpBinsOpts,
  mkSubCommand,
 )
import App.Fossa.EmbeddedBinary (allBins, dumpEmbeddedBinary)
import App.Fossa.Subcommand (SubCommand)
import Control.Effect.Lift (Has, Lift)
import Data.Foldable (for_)

dumpSubCommand :: SubCommand DumpBinsOpts DumpBinsConfig
dumpSubCommand = mkSubCommand dumpBinsMain

dumpBinsMain :: Has (Lift IO) sig m => DumpBinsConfig -> m ()
dumpBinsMain (DumpBinsConfig dir) = for_ allBins $ dumpEmbeddedBinary dir
