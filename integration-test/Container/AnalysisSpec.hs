{-# LANGUAGE OverloadedRecordDot #-}

module Container.AnalysisSpec (spec) where

import App.Fossa.Config.Common (ScanDestination (OutputStdout))
import App.Fossa.Config.Container.Analyze (ContainerAnalyzeConfig (..))
import App.Fossa.Config.Container.Common (ImageText (ImageText))
import App.Fossa.Container.AnalyzeNative (analyzeExperimental)
import App.Types (OverrideProject (OverrideProject))
import Container.FixtureUtils (runContainerEffs)
import Container.Types (
  ContainerScan (imageData, imageTag),
  ContainerScanImage (imageLayers, imageOs, imageOsRelease),
 )
import Data.Flag (toFlag')
import Diag.Result (Result (..))
import Effect.Logger (Severity (SevInfo))
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "Container Scanning" registrySourceAnalysis

registrySourceCfg :: ContainerAnalyzeConfig
registrySourceCfg =
  ContainerAnalyzeConfig
    { scanDestination = OutputStdout
    , revisionOverride = OverrideProject Nothing Nothing Nothing
    , imageLocator = ImageText "ghcr.io/fossas/haskell-dev-tools:9.8.4"
    , jsonOutput = toFlag' False
    , usesExperimentalScanner = True
    , dockerHost = ""
    , arch = "amd64"
    , severity = SevInfo
    , onlySystemDeps = False
    , filterSet = mempty
    , withoutDefaultFilters = toFlag' False
    , debugDir = Nothing
    }

runAnalyze :: ContainerAnalyzeConfig -> (ContainerScan -> IO ()) -> IO ()
runAnalyze analyzeCfg action = do
  res <- runContainerEffs (analyzeExperimental analyzeCfg)
  case res of
    Failure _ errGroup -> fail . show $ errGroup
    Success _ a -> action a

registrySourceAnalysis :: Spec
registrySourceAnalysis = do
  aroundAll (runAnalyze registrySourceCfg) $ do
    describe "Container analysis from registry source" $ do
      it "Has the correct OS" $
        \scan -> scan.imageData.imageOs `shouldBe` Just "alpine"
      it "Has the correct OS release version" $
        \scan -> scan.imageData.imageOsRelease `shouldBe` Just "3.18.12"
      it "Has the expected image tag" $
        \scan -> scan.imageTag `shouldBe` "ghcr.io/fossas/haskell-dev-tools"
      it "Has at least one layer" $
        \scan -> scan.imageData.imageLayers `shouldSatisfy` (not . null)
