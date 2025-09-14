{-# LANGUAGE OverloadedStrings #-}

module App.Fossa.ConfidenceSpec (spec) where

import App.Fossa.Confidence
import Test.Hspec

spec :: Spec
spec = describe "Confidence Analysis" $ do
  describe "calculateConfidence" $ do
    let
      manifestPassed = ValidationFactor ManifestConsistency True 30 "desc"
      manifestFailed = ValidationFactor ManifestConsistency False 30 "desc"
      binaryPassed = ValidationFactor BinaryPresence True 25 "desc"
      binaryFailed = ValidationFactor BinaryPresence False 25 "desc"
      sourcePassed = ValidationFactor SourceCodeUsage True 45 "desc"
      sourceFailed = ValidationFactor SourceCodeUsage False 45 "desc"

    it "returns 100 when all factors pass" $ do
      let factors = [manifestPassed, binaryPassed, sourcePassed]
      calculateConfidence factors `shouldBe` ConfidenceScore 100

    it "returns 0 when all factors fail" $ do
      let factors = [manifestFailed, binaryFailed, sourceFailed]
      calculateConfidence factors `shouldBe` ConfidenceScore 0

    it "correctly calculates weighted score for mixed results" $ do
      let factors = [manifestFailed, binaryPassed, sourcePassed] -- 70 points (25 + 45)
      calculateConfidence factors `shouldBe` ConfidenceScore 70

    it "correctly calculates a different weighted score" $ do
      let factors = [manifestPassed, binaryFailed, sourcePassed] -- 75 points (30 + 45)
      calculateConfidence factors `shouldBe` ConfidenceScore 75

    it "handles cases with zero total weight" $ do
      calculateConfidence [] `shouldBe` ConfidenceScore 0