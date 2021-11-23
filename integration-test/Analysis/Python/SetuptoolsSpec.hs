{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.Python.SetuptoolsSpec (spec) where
import Analysis.Utils
import Path
import Strategy.Python.Setuptools qualified as Setuptools
import Test.Hspec
import Types (GraphBreadth (..))

spec :: Spec
spec = do
  beforeWith performDiscoveryAndAnalyses (AnalysisIntegrationCase "theFuck" Setuptools.discover LocalEnvironment Nothing FixtureArtifact "https://github.com/nvbn/thefuck/archive/refs/tags/3.31.tar.gz" [reldir|thefuck/|] []) $
    describe "exampleDescription" $
      it "exampleTest" $ do
        res `shouldBe` Nothing
        1 `shouldBe` 1