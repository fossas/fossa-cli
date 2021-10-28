module App.Fossa.AnalyzeSpec (spec) where

import App.Fossa.Analyze (DiscoverFunc, discoverFuncs)
import Control.Carrier.Debug (DebugC)
import Control.Carrier.Diagnostics (DiagnosticsC)
import Effect.Exec (ExecIOC)
import Effect.Logger (LoggerC)
import Effect.ReadFS (ReadFSIOC)
import Test.Hspec (Spec, describe, it, shouldBe)

type SomeMonad = DebugC (DiagnosticsC (LoggerC (ExecIOC (ReadFSIOC IO))))

spec :: Spec
spec =
  -- this test only exists to prevent merging the commented out analyzers
  describe "Discovery function list" $
    it "should be length 32" $
      length (discoverFuncs :: [DiscoverFunc SomeMonad]) `shouldBe` 32
