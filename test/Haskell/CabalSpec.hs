module Haskell.CabalSpec
  ( spec,
  )
where

import Control.Carrier.Diagnostics
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as S
import DepTypes
import GraphUtil
import Graphing
import Prologue
import Strategy.Haskell.Cabal
import qualified Test.Hspec as Test

buildPlan :: [InstallPlan]
buildPlan = [aesonPlan, basePlan, deepDepPlan, rtsPlan, spectrometerFossaPlan, spectrometerLibPlan, withCompPlan]

aesonId, baseId, deepDepId, rtsId, spectrometerFossaId, spectrometerLibId, withCompId :: PlanId
aesonId = PlanId "aeson-1.5.2.0-abcd"
baseId = PlanId "base-4.13.0.0"
deepDepId = PlanId "deepdep-nonsense-tag"
rtsId = PlanId "rts"
spectrometerFossaId = PlanId "spectrometer-0.1.0.0-inplace-fossa"
spectrometerLibId = PlanId "spectrometer-0.1.0.0-inplace"
withCompId = PlanId "with-components-1.0.2.3-efgh"

aesonPlan, basePlan, deepDepPlan, rtsPlan, spectrometerFossaPlan, spectrometerLibPlan, withCompPlan :: InstallPlan
aesonPlan = InstallPlan Configured aesonId "aeson" "1.5.2.0" (S.fromList [baseId, deepDepId]) (Just Global) mempty
basePlan = InstallPlan PreExisting baseId "base" "4.13.0.0" (S.fromList [rtsId]) Nothing mempty
deepDepPlan = InstallPlan Configured deepDepId "deepdep" "3.2.1.0" (S.fromList [baseId]) (Just Global) mempty
rtsPlan = InstallPlan PreExisting rtsId "rts" "1.0" mempty Nothing mempty
spectrometerFossaPlan = InstallPlan Configured spectrometerFossaId "spectrometer" "0.1.0.0" (S.fromList [baseId, spectrometerLibId]) (Just Local) mempty
spectrometerLibPlan = InstallPlan Configured spectrometerLibId "spectrometer" "0.1.0.0" (S.fromList [aesonId, baseId, withCompId]) (Just Local) mempty
withCompPlan = InstallPlan Configured withCompId "with-components" "1.0.2.3" mempty (Just Global) (S.fromList [baseId, rtsId])

spec :: Test.Spec
spec = do
  Test.describe "cabal plan parser" $ do
    metaBytes <- Test.runIO $ BL.readFile "test/Haskell/testdata/cabal-plan.json"
    Test.it "should parse a cabal plan correctly" $
      case eitherDecode metaBytes of
        Left err -> Test.expectationFailure $ "failed to parse: " ++ err
        Right result -> installPlans result `Test.shouldMatchList` buildPlan
  Test.describe "cabal plan graph builder" $
    case run . runDiagnostics . buildGraph $ BuildPlan buildPlan of
      Left fbundle -> Test.it "should build a graph" $ Test.expectationFailure (show $ renderFailureBundle fbundle)
      Right ResultBundle {..} -> do
        let gr = gmap dependencyName resultValue
        Test.it "should have the correct deps" $ expectDeps ["aeson", "with-components", "deepdep"] gr
        Test.it "should have the correct direct deps" $ expectDirect ["aeson", "with-components"] gr
        Test.it "should have the correct edges" $ expectEdges [("aeson", "deepdep")] gr
