module App.Fossa.AnalyzeSpec (spec) where

import App.Fossa.Analyze (DiscoverFunc, discoverFuncs)
import App.Fossa.Analyze.Types (AnalyzeExperimentalPreferences)
import Control.Carrier.Debug (DebugC)
import Control.Carrier.Diagnostics (DiagnosticsC)
import Control.Carrier.Reader (ReaderC)
import Effect.Exec (ExecIOC)
import Effect.Logger (LoggerC)
import Effect.ReadFS (ReadFSIOC)
import Test.Hspec (Spec, describe, it, shouldBe)

type SomeMonad = ReaderC AnalyzeExperimentalPreferences (DebugC (DiagnosticsC (LoggerC (ExecIOC (ReadFSIOC IO)))))

spec :: Spec
spec =
  -- this test only exists to prevent merging the commented out analyzers
  describe "Discovery function list" $
    it "should be length 34" $
      length (discoverFuncs :: [DiscoverFunc SomeMonad]) `shouldBe` 34
