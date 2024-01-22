module App.Fossa.AnalyzeSpec (spec) where

import App.Fossa.Analyze.Discover (DiscoverFunc, discoverFuncs)
import App.Fossa.Config.Analyze (ExperimentalAnalyzeConfig)
import App.Types (OverrideDynamicAnalysisBinary)
import Control.Carrier.Debug (DebugC)
import Control.Carrier.Diagnostics (DiagnosticsC)
import Control.Carrier.Reader (ReaderC)
import Control.Carrier.Stack (StackC)
import Control.Carrier.Telemetry (TelemetryC)
import Discovery.Filters (AllFilters, MavenScopeFilters)
import Effect.Exec (ExecIOC)
import Effect.Logger (LoggerC)
import Effect.ReadFS (ReadFSIOC)
import Test.Hspec (Spec, describe, it, shouldBe)
import Type.Operator (type ($))

type SomeMonad = TelemetryC $ ReaderC OverrideDynamicAnalysisBinary $ ReaderC ExperimentalAnalyzeConfig $ ReaderC MavenScopeFilters $ ReaderC AllFilters $ DebugC $ DiagnosticsC $ LoggerC $ ExecIOC $ ReadFSIOC $ StackC IO

spec :: Spec
spec =
  -- this test only exists to prevent merging the commented out analyzers
  describe "Discovery function list" $
    it "should be length 35" $
      length (discoverFuncs :: [DiscoverFunc SomeMonad]) `shouldBe` 36
