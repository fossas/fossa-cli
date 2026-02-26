module App.Fossa.CryptoScan.FipsReportSpec (spec) where

import App.Fossa.CryptoScan.FipsReport (FipsReportStats (..), computeFipsStats)
import App.Fossa.CryptoScan.Types (
  Confidence (..),
  CryptoAlgorithm (..),
  CryptoFinding (..),
  CryptoPrimitive (..),
  CryptoScanResults (..),
  DetectionMethod (..),
  FipsStatus (..),
 )
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "CryptoScan FIPS Report" $ do
  describe "computeFipsStats" $ do
    it "returns zero stats for empty results" $ do
      let stats = computeFipsStats (CryptoScanResults [])
      totalAlgorithms stats `shouldBe` 0
      approvedCount stats `shouldBe` 0
      deprecatedCount stats `shouldBe` 0
      notApprovedCount stats `shouldBe` 0

    it "correctly counts approved algorithms" $ do
      let results =
            CryptoScanResults
              [ mkFinding "AES-256-GCM" FipsApproved
              , mkFinding "SHA-256" FipsApproved
              ]
          stats = computeFipsStats results
      totalAlgorithms stats `shouldBe` 2
      approvedCount stats `shouldBe` 2
      deprecatedCount stats `shouldBe` 0
      notApprovedCount stats `shouldBe` 0

    it "correctly counts mixed statuses" $ do
      let results =
            CryptoScanResults
              [ mkFinding "AES-256-GCM" FipsApproved
              , mkFinding "SHA-1" FipsDeprecated
              , mkFinding "ChaCha20-Poly1305" FipsNotApproved
              , mkFinding "MD5" FipsNotApproved
              ]
          stats = computeFipsStats results
      totalAlgorithms stats `shouldBe` 4
      approvedCount stats `shouldBe` 1
      deprecatedCount stats `shouldBe` 1
      notApprovedCount stats `shouldBe` 2

    it "deduplicates algorithms by name" $ do
      let results =
            CryptoScanResults
              [ mkFinding "AES-256-GCM" FipsApproved
              , mkFinding "AES-256-GCM" FipsApproved -- duplicate
              , mkFinding "SHA-256" FipsApproved
              ]
          stats = computeFipsStats results
      totalAlgorithms stats `shouldBe` 2
      approvedCount stats `shouldBe` 2

-- Test fixtures

mkAlgorithm :: Text -> FipsStatus -> CryptoAlgorithm
mkAlgorithm name status =
  CryptoAlgorithm
    { cryptoAlgorithmName = name
    , cryptoAlgorithmFamily = "test"
    , cryptoAlgorithmPrimitive = PrimitiveAe
    , cryptoAlgorithmParameterSet = Nothing
    , cryptoAlgorithmEllipticCurve = Nothing
    , cryptoAlgorithmMode = Nothing
    , cryptoAlgorithmOid = Nothing
    , cryptoAlgorithmClassicalSecurityLevel = Nothing
    , cryptoAlgorithmNistQuantumSecurityLevel = 0
    , cryptoAlgorithmFipsStatus = status
    , cryptoAlgorithmCryptoFunctions = []
    }

mkFinding :: Text -> FipsStatus -> CryptoFinding
mkFinding name status =
  CryptoFinding
    { cryptoFindingAlgorithm = mkAlgorithm name status
    , cryptoFindingFilePath = "test.py"
    , cryptoFindingLineNumber = 1
    , cryptoFindingMatchedText = name
    , cryptoFindingDetectionMethod = ApiCall
    , cryptoFindingEcosystem = "python"
    , cryptoFindingProvidingLibrary = Just "cryptography"
    , cryptoFindingConfidence = ConfidenceHigh
    }
