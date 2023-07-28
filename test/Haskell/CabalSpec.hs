module Haskell.CabalSpec (
  spec,
) where

import Control.Carrier.Diagnostics
import Control.Carrier.Stack (runStack)
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Set qualified as Set
import DepTypes
import Effect.Logger (Severity (SevError), withDefaultLogger)
import GraphUtil
import Graphing
import ResultUtil
import Strategy.Haskell.Cabal
import Test.Hspec (runIO)
import Test.Hspec qualified as Test

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
aesonPlan = InstallPlan Configured aesonId "aeson" "1.5.2.0" (Set.fromList [baseId, deepDepId]) (Just Global) mempty
basePlan = InstallPlan PreExisting baseId "base" "4.13.0.0" (Set.fromList [rtsId]) Nothing mempty
deepDepPlan = InstallPlan Configured deepDepId "deepdep" "3.2.1.0" (Set.fromList [baseId]) (Just Global) mempty
rtsPlan = InstallPlan PreExisting rtsId "rts" "1.0" mempty Nothing mempty
spectrometerFossaPlan = InstallPlan Configured spectrometerFossaId "spectrometer" "0.1.0.0" (Set.fromList [baseId, spectrometerLibId]) (Just Local) mempty
spectrometerLibPlan = InstallPlan Configured spectrometerLibId "spectrometer" "0.1.0.0" (Set.fromList [aesonId, baseId, withCompId]) (Just Local) mempty
withCompPlan = InstallPlan Configured withCompId "with-components" "1.0.2.3" mempty (Just Global) (Set.fromList [baseId, rtsId])

spec :: Test.Spec
spec = do
  Test.describe "cabal plan parser" $ do
    metaBytes <- Test.runIO $ BL.readFile "test/Haskell/testdata/cabal-plan.json"
    Test.it "should parse a cabal plan correctly" $
      case eitherDecode metaBytes of
        Left err -> Test.expectationFailure $ "failed to parse: " ++ err
        Right result -> installPlans result `Test.shouldMatchList` buildPlan

  Test.describe "cabal plan graph builder" $ do
    result <- runIO . runStack . withDefaultLogger SevError . runDiagnostics . buildGraph $ BuildPlan buildPlan

    Test.it "should build a correct graph" $
      assertOnSuccess result $ \_ graph -> do
        let gr = gmap dependencyName graph
        -- These values are likely not correct long term, but are modified based on the fact that
        -- `shouldInclude` is currently set to always include any reported dependency.
        expectDeps ["aeson", "base", "deepdep", "rts", "spectrometer", "with-components"] gr
        expectDirect ["aeson", "base", "spectrometer", "with-components"] gr
        expectEdges expectedEdges gr
  where
    expectedEdges =
      [ ("aeson", "deepdep")
      , ("aeson", "base")
      , ("base", "rts")
      , ("deepdep", "base")
      , ("spectrometer", "base")
      , ("spectrometer", "spectrometer")
      , ("spectrometer", "aeson")
      , ("spectrometer", "base")
      , ("spectrometer", "with-components")
      , ("with-components", "base")
      ]
