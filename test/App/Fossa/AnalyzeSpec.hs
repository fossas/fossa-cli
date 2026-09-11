module App.Fossa.AnalyzeSpec (spec) where

import App.Fossa.Analyze (sendToDestination)
import App.Fossa.Analyze.Discover (DiscoverFunc, discoverFuncs)
import App.Fossa.Config.Analyze (StrategyConfig)
import App.Fossa.Config.Common (DestinationMeta (DestinationMeta), ScanDestination (..))
import App.Types (Mode, OverrideDynamicAnalysisBinary)
import Control.Carrier.Debug (DebugC)
import Control.Carrier.Diagnostics (DiagnosticsC)
import Control.Carrier.Reader (ReaderC)
import Control.Carrier.Stack (StackC)
import Control.Carrier.State.Strict (runState)
import Control.Carrier.Telemetry (TelemetryC)
import Control.Effect.State (modify)
import Data.Aeson qualified as Aeson
import Discovery.Filters (AllFilters, MavenScopeFilters)
import Effect.Exec (ExecIOC)
import Effect.Logger (LoggerC)
import Effect.ReadFS (ReadFSIOC)
import Test.Effect (it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, it, shouldBe)
import Type.Operator (type ($))

type SomeMonad = TelemetryC $ ReaderC OverrideDynamicAnalysisBinary $ ReaderC StrategyConfig $ ReaderC MavenScopeFilters $ ReaderC Mode $ ReaderC AllFilters $ DebugC $ DiagnosticsC $ LoggerC $ ExecIOC $ ReadFSIOC $ StackC IO

spec :: Spec
spec = do
  -- this test only exists to prevent merging the commented out analyzers
  describe "Discovery function list" $
    it "should be length 36" $
      length (discoverFuncs :: [DiscoverFunc SomeMonad]) `shouldBe` 36

  sendToDestinationSpec

-- | Runs under the mock API with no expectations, so any request the stdout
-- branch made would fail the test on top of the upload count.
sendToDestinationSpec :: Spec
sendToDestinationSpec = describe "sendToDestination" $ do
  let meta = DestinationMeta (Fixtures.apiOpts, Fixtures.projectMetadata)
      result = Aeson.object []
      countUploads destination = runState (0 :: Int) $ sendToDestination (\_ -> modify ((+ 1) :: Int -> Int)) destination result

  it' "uploads nothing when the destination is stdout" $ do
    (uploads, ()) <- countUploads OutputStdout
    uploads `shouldBe'` 0

  it' "uploads once for an upload destination" $ do
    (uploads, ()) <- countUploads (UploadScan meta)
    uploads `shouldBe'` 1

  it' "uploads once when also writing to stdout" $ do
    (uploads, ()) <- countUploads (OutputAndUpload meta)
    uploads `shouldBe'` 1
