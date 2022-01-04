module App.Fossa.VPS (vpsSubCommand) where

import App.Fossa.VPS.AOSPNotice qualified as AOSP
import App.Fossa.VPS.Report qualified as Report
import App.Fossa.VPS.Scan qualified as Analyze
import App.Fossa.VPS.Test qualified as Test
import App.Fossa.Config.VPS (VPSCliOpts, VPSConfig (..))
import App.Fossa.Config.VPS qualified as Config
import App.Fossa.Subcommand (SubCommand)
import Control.Effect.Diagnostics (Diagnostics, Has)
import Control.Effect.Lift (Lift)
import Effect.Exec (Exec)
import Effect.Logger (Logger)

vpsSubCommand :: SubCommand VPSCliOpts VPSConfig
vpsSubCommand = Config.mkSubCommand dispatch

dispatch ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  ) =>
  VPSConfig ->
  m ()
dispatch = \case
  AnalyzeCfg cfg -> Analyze.scanMain cfg
  AOSPNoticeCfg cfg -> AOSP.aospNoticeMain cfg
  TestCfg cfg -> Test.testMain cfg
  ReportCfg cfg -> Report.reportMain cfg
