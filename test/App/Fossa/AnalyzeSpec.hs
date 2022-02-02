module App.Fossa.AnalyzeSpec (spec) where

import App.Fossa.Analyze.Discover (DiscoverFunc, discoverFuncs)
import App.Fossa.Config.Analyze (ExperimentalAnalyzeConfig)
import Control.Carrier.Debug (DebugC)
import Control.Carrier.Diagnostics (DiagnosticsC)
import Control.Carrier.Reader (ReaderC)
import Control.Carrier.Stack (StackC)
import Effect.Exec (ExecIOC)
import Effect.Logger (LoggerC)
import Effect.ReadFS (ReadFSIOC)
import Test.Hspec (Spec, describe, it, shouldBe)

type SomeMonad = ReaderC ExperimentalAnalyzeConfig (DebugC (DiagnosticsC (LoggerC (ExecIOC (ReadFSIOC (StackC IO))))))

spec :: Spec
spec =
  -- this test only exists to prevent merging the commented out analyzers
  describe "Discovery function list" $
    it "should be length 34" $
      length (discoverFuncs :: [DiscoverFunc SomeMonad]) `shouldBe` 34
