module App.Fossa.LicenseScan (
  licenseScanSubCommand,
) where

import App.Fossa.Config.LicenseScan (
  LicenseScanConfig (..),
  LicenseScanOpts,
  mkSubCommand,
 )
import App.Fossa.EmbeddedBinary (withThemisAndIndex)
import App.Fossa.RunThemis (execRawThemis)
import App.Fossa.Subcommand (SubCommand)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Has, Lift)
import Data.String.Conversion (decodeUtf8)
import Effect.Exec (Exec)
import Effect.Logger (Logger, logStdout)

licenseScanSubCommand :: SubCommand LicenseScanOpts LicenseScanConfig
licenseScanSubCommand = mkSubCommand licenseScanMain

licenseScanMain ::
  ( Has (Lift IO) sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  LicenseScanConfig ->
  m ()
licenseScanMain (LicenseScanConfig dir) = logStdout . decodeUtf8 =<< withThemisAndIndex (`execRawThemis` dir)
