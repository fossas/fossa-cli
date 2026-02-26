{-# LANGUAGE RecordWildCards #-}

module App.Fossa.CryptoScan.Types (
  CryptoScanResults (..),
  CryptoFinding (..),
  CryptoAlgorithm (..),
  FipsStatus (..),
  DetectionMethod (..),
  Confidence (..),
  CryptoPrimitive (..),
) where

import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (String),
  object,
  withObject,
  withText,
  (.:),
  (.:?),
  (.=),
 )
import Data.Text (Text)

-- | Wrapper for the list of crypto findings returned by the cryptoscan binary.
newtype CryptoScanResults = CryptoScanResults
  { cryptoFindings :: [CryptoFinding]
  }
  deriving (Show, Eq, Ord)

instance FromJSON CryptoScanResults where
  parseJSON v = CryptoScanResults <$> parseJSON v

instance ToJSON CryptoScanResults where
  toJSON (CryptoScanResults findings) = toJSON findings

-- | A single crypto finding: an algorithm detected at a specific location.
data CryptoFinding = CryptoFinding
  { cryptoFindingAlgorithm :: CryptoAlgorithm
  , cryptoFindingFilePath :: Text
  , cryptoFindingLineNumber :: Int
  , cryptoFindingMatchedText :: Text
  , cryptoFindingDetectionMethod :: DetectionMethod
  , cryptoFindingEcosystem :: Text
  , cryptoFindingProvidingLibrary :: Maybe Text
  , cryptoFindingConfidence :: Confidence
  }
  deriving (Show, Eq, Ord)

instance FromJSON CryptoFinding where
  parseJSON = withObject "CryptoFinding" $ \o ->
    CryptoFinding
      <$> o .: "algorithm"
      <*> o .: "file_path"
      <*> o .: "line_number"
      <*> o .: "matched_text"
      <*> o .: "detection_method"
      <*> o .: "ecosystem"
      <*> o .:? "providing_library"
      <*> o .: "confidence"

instance ToJSON CryptoFinding where
  toJSON CryptoFinding{..} =
    object
      [ "algorithm" .= cryptoFindingAlgorithm
      , "file_path" .= cryptoFindingFilePath
      , "line_number" .= cryptoFindingLineNumber
      , "matched_text" .= cryptoFindingMatchedText
      , "detection_method" .= cryptoFindingDetectionMethod
      , "ecosystem" .= cryptoFindingEcosystem
      , "providing_library" .= cryptoFindingProvidingLibrary
      , "confidence" .= cryptoFindingConfidence
      ]

-- | A detected cryptographic algorithm with all its metadata.
data CryptoAlgorithm = CryptoAlgorithm
  { cryptoAlgorithmName :: Text
  , cryptoAlgorithmFamily :: Text
  , cryptoAlgorithmPrimitive :: CryptoPrimitive
  , cryptoAlgorithmParameterSet :: Maybe Text
  , cryptoAlgorithmEllipticCurve :: Maybe Text
  , cryptoAlgorithmMode :: Maybe Text
  , cryptoAlgorithmOid :: Maybe Text
  , cryptoAlgorithmClassicalSecurityLevel :: Maybe Int
  , cryptoAlgorithmNistQuantumSecurityLevel :: Int
  , cryptoAlgorithmFipsStatus :: FipsStatus
  , cryptoAlgorithmCryptoFunctions :: [Text]
  }
  deriving (Show, Eq, Ord)

instance FromJSON CryptoAlgorithm where
  parseJSON = withObject "CryptoAlgorithm" $ \o ->
    CryptoAlgorithm
      <$> o .: "name"
      <*> o .: "algorithm_family"
      <*> o .: "primitive"
      <*> o .:? "parameter_set"
      <*> o .:? "elliptic_curve"
      <*> o .:? "mode"
      <*> o .:? "oid"
      <*> o .:? "classical_security_level"
      <*> o .: "nist_quantum_security_level"
      <*> o .: "fips_status"
      <*> o .: "crypto_functions"

instance ToJSON CryptoAlgorithm where
  toJSON CryptoAlgorithm{..} =
    object
      [ "name" .= cryptoAlgorithmName
      , "algorithm_family" .= cryptoAlgorithmFamily
      , "primitive" .= cryptoAlgorithmPrimitive
      , "parameter_set" .= cryptoAlgorithmParameterSet
      , "elliptic_curve" .= cryptoAlgorithmEllipticCurve
      , "mode" .= cryptoAlgorithmMode
      , "oid" .= cryptoAlgorithmOid
      , "classical_security_level" .= cryptoAlgorithmClassicalSecurityLevel
      , "nist_quantum_security_level" .= cryptoAlgorithmNistQuantumSecurityLevel
      , "fips_status" .= cryptoAlgorithmFipsStatus
      , "crypto_functions" .= cryptoAlgorithmCryptoFunctions
      ]

-- | FIPS compliance status for a cryptographic algorithm.
-- Serialized as kebab-case to match Rust serde output.
data FipsStatus
  = FipsApproved
  | FipsDeprecated
  | FipsNotApproved
  deriving (Show, Eq, Ord)

instance FromJSON FipsStatus where
  parseJSON = withText "FipsStatus" $ \case
    "approved" -> pure FipsApproved
    "deprecated" -> pure FipsDeprecated
    "not-approved" -> pure FipsNotApproved
    other -> fail $ "Unknown FipsStatus: " <> show other

instance ToJSON FipsStatus where
  toJSON FipsApproved = String "approved"
  toJSON FipsDeprecated = String "deprecated"
  toJSON FipsNotApproved = String "not-approved"

-- | Detection method used to identify the crypto usage.
data DetectionMethod
  = DependencyManifest
  | ImportStatement
  | ApiCall
  | ConfigFile
  | StringLiteral
  deriving (Show, Eq, Ord)

instance FromJSON DetectionMethod where
  parseJSON = withText "DetectionMethod" $ \case
    "dependency-manifest" -> pure DependencyManifest
    "import-statement" -> pure ImportStatement
    "api-call" -> pure ApiCall
    "config-file" -> pure ConfigFile
    "string-literal" -> pure StringLiteral
    other -> fail $ "Unknown DetectionMethod: " <> show other

instance ToJSON DetectionMethod where
  toJSON DependencyManifest = String "dependency-manifest"
  toJSON ImportStatement = String "import-statement"
  toJSON ApiCall = String "api-call"
  toJSON ConfigFile = String "config-file"
  toJSON StringLiteral = String "string-literal"

-- | Confidence level of the detection.
data Confidence
  = ConfidenceHigh
  | ConfidenceMedium
  | ConfidenceLow
  deriving (Show, Eq, Ord)

instance FromJSON Confidence where
  parseJSON = withText "Confidence" $ \case
    "high" -> pure ConfidenceHigh
    "medium" -> pure ConfidenceMedium
    "low" -> pure ConfidenceLow
    other -> fail $ "Unknown Confidence: " <> show other

instance ToJSON Confidence where
  toJSON ConfidenceHigh = String "high"
  toJSON ConfidenceMedium = String "medium"
  toJSON ConfidenceLow = String "low"

-- | Cryptographic primitive type (maps to CycloneDX 1.7 enum).
data CryptoPrimitive
  = PrimitiveAe
  | PrimitiveBlockCipher
  | PrimitiveStreamCipher
  | PrimitiveHash
  | PrimitiveMac
  | PrimitiveSignature
  | PrimitivePke
  | PrimitiveKem
  | PrimitiveKeyAgree
  | PrimitiveKdf
  | PrimitiveXof
  | PrimitiveDrbg
  | PrimitiveCombiner
  | PrimitiveOther
  | PrimitiveUnknown
  deriving (Show, Eq, Ord)

instance FromJSON CryptoPrimitive where
  parseJSON = withText "CryptoPrimitive" $ \case
    "ae" -> pure PrimitiveAe
    "block-cipher" -> pure PrimitiveBlockCipher
    "stream-cipher" -> pure PrimitiveStreamCipher
    "hash" -> pure PrimitiveHash
    "mac" -> pure PrimitiveMac
    "signature" -> pure PrimitiveSignature
    "pke" -> pure PrimitivePke
    "kem" -> pure PrimitiveKem
    "key-agree" -> pure PrimitiveKeyAgree
    "kdf" -> pure PrimitiveKdf
    "xof" -> pure PrimitiveXof
    "drbg" -> pure PrimitiveDrbg
    "combiner" -> pure PrimitiveCombiner
    "other" -> pure PrimitiveOther
    "unknown" -> pure PrimitiveUnknown
    other -> fail $ "Unknown CryptoPrimitive: " <> show other

instance ToJSON CryptoPrimitive where
  toJSON PrimitiveAe = String "ae"
  toJSON PrimitiveBlockCipher = String "block-cipher"
  toJSON PrimitiveStreamCipher = String "stream-cipher"
  toJSON PrimitiveHash = String "hash"
  toJSON PrimitiveMac = String "mac"
  toJSON PrimitiveSignature = String "signature"
  toJSON PrimitivePke = String "pke"
  toJSON PrimitiveKem = String "kem"
  toJSON PrimitiveKeyAgree = String "key-agree"
  toJSON PrimitiveKdf = String "kdf"
  toJSON PrimitiveXof = String "xof"
  toJSON PrimitiveDrbg = String "drbg"
  toJSON PrimitiveCombiner = String "combiner"
  toJSON PrimitiveOther = String "other"
  toJSON PrimitiveUnknown = String "unknown"
