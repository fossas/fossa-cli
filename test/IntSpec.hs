{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.Python.SetuptoolsSpec (spec) where
import Utils
import Path
import Strategy.Python.Setuptools qualified as Setuptools
import Test.Hspec
import Types (GraphBreadth (..))


theFuck = AnalysisIntegrationCase "theFuck" Setuptools.discover LocalEnvironment Nothing $ FixtureArtifact "https://github.com/nvbn/thefuck/archive/refs/tags/3.31.tar.gz" [reldir|thefuck/|]

spec :: Spec
spec = do
  before (performDiscoveryAndAnalyses theFuck) $
    describe "exampleDescription" $
      it "has a db and a user" $ \(db) -> do
        res `shouldBe` Nothing