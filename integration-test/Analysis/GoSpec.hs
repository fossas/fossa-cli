{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.GoSpec (spec) where

import Analysis.FixtureExpectationUtils
import Analysis.FixtureUtils
import App.Types (Mode (NonStrict))
import Path
import Strategy.Gomodules qualified as Gomodules
import Test.Hspec
import Types (DiscoveredProjectType (..))

goEnv :: FixtureEnvironment
goEnv = NixEnv ["go"]

vault :: AnalysisTestFixture (Gomodules.GomodulesProject)
vault =
  AnalysisTestFixture
    "vault"
    Gomodules.discover
    goEnv
    Nothing
    $ FixtureArtifact
      "https://github.com/hashicorp/vault/archive/refs/tags/v1.9.1.tar.gz"
      [reldir|go/vault/|]
      [reldir|vault-1.9.1/|]

testVault :: Spec
testVault =
  aroundAll (withAnalysisOf NonStrict vault) $ do
    describe "vault" $ do
      it "should find targets" $ \(result, extractedDir) -> do
        expectProject (GomodProjectType, extractedDir) result
        length result `shouldBe` 7

spec :: Spec
spec = do
  testVault
