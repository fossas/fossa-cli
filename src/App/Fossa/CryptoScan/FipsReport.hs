{-# LANGUAGE RecordWildCards #-}

module App.Fossa.CryptoScan.FipsReport (
  renderFipsReport,
  FipsReportStats (..),
  computeFipsStats,
) where

import App.Fossa.CryptoScan.Types (
  CryptoAlgorithm (..),
  CryptoFinding (..),
  CryptoPrimitive (..),
  CryptoScanResults (..),
  FipsStatus (..),
 )
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter (
  Doc,
  Pretty (pretty),
  annotate,
  hardline,
  indent,
  vcat,
  vsep,
 )
import Prettyprinter.Render.Terminal (
  AnsiStyle,
  Color (Green, Red, Yellow),
  bold,
  color,
 )

data FipsReportStats = FipsReportStats
  { totalAlgorithms :: Int
  , approvedCount :: Int
  , deprecatedCount :: Int
  , notApprovedCount :: Int
  }
  deriving (Show, Eq)

compliancePercentage :: FipsReportStats -> Int
compliancePercentage FipsReportStats{..} =
  if totalAlgorithms == 0
    then 100
    else (approvedCount * 100) `div` totalAlgorithms

-- | Dedupe key for algorithm variants — includes parameter set so that
-- e.g. RSA-2048 and RSA-1024 are counted separately.
dedupeKey :: CryptoFinding -> (Text, Maybe Text)
dedupeKey finding =
  let algo = cryptoFindingAlgorithm finding
   in (Text.toCaseFold (cryptoAlgorithmName algo), cryptoAlgorithmParameterSet algo)

-- | Deduplicate findings by (name, parameterSet), keeping the first occurrence.
-- Uses a Map for O(n log n) rather than nubBy's O(n^2).
deduplicateFindings :: [CryptoFinding] -> [CryptoFinding]
deduplicateFindings = Map.elems . foldl' insertFirst Map.empty
  where
    insertFirst acc finding =
      let key = dedupeKey finding
       in if Map.member key acc then acc else Map.insert key finding acc

computeFipsStats :: CryptoScanResults -> FipsReportStats
computeFipsStats (CryptoScanResults findings) =
  computeFipsStatsFromFindings (deduplicateFindings findings)

-- | Compute FIPS stats from already-deduplicated findings.
computeFipsStatsFromFindings :: [CryptoFinding] -> FipsReportStats
computeFipsStatsFromFindings uniqueAlgos =
  let statuses = map (cryptoAlgorithmFipsStatus . cryptoFindingAlgorithm) uniqueAlgos
   in FipsReportStats
        { totalAlgorithms = length uniqueAlgos
        , approvedCount = length $ filter (== FipsApproved) statuses
        , deprecatedCount = length $ filter (== FipsDeprecated) statuses
        , notApprovedCount = length $ filter (== FipsNotApproved) statuses
        }

-- | Render a comprehensive FIPS compliance report from crypto scan results.
renderFipsReport :: CryptoScanResults -> Doc AnsiStyle
renderFipsReport (CryptoScanResults findings) =
  let uniqueFindings = deduplicateFindings findings
      stats = computeFipsStatsFromFindings uniqueFindings
      categorized = categorizeFindings uniqueFindings
   in vsep
        [ annotate bold "FIPS Compliance Report"
        , annotate bold "====================="
        , ""
        , renderSummary stats
        , ""
        , renderCategoryBreakdown categorized
        , ""
        , renderRemediationTable uniqueFindings
        , ""
        , renderKeySizeWarnings uniqueFindings
        ]

renderSummary :: FipsReportStats -> Doc AnsiStyle
renderSummary stats@FipsReportStats{..} =
  vcat
    [ annotate bold "Summary"
    , annotate bold "-------"
    , "Total unique algorithms detected: " <> pretty totalAlgorithms
    , annotate (color Green) $ "  FIPS Approved:     " <> pretty approvedCount
    , annotate (color Yellow) $ "  FIPS Deprecated:   " <> pretty deprecatedCount
    , annotate (color Red) $ "  Not FIPS Approved: " <> pretty notApprovedCount
    , ""
    , "Overall compliance: " <> coloredPercentage (compliancePercentage stats)
    ]

coloredPercentage :: Int -> Doc AnsiStyle
coloredPercentage pct
  | pct >= 100 = annotate (color Green) $ pretty pct <> "%"
  | pct >= 80 = annotate (color Yellow) $ pretty pct <> "%"
  | otherwise = annotate (color Red) $ pretty pct <> "%"

-- Category types for grouping
data CryptoCategory
  = CatSymmetricEncryption
  | CatHashFunctions
  | CatMacs
  | CatAsymmetricSignatures
  | CatKeyExchange
  | CatKdfs
  | CatOther
  deriving (Eq, Ord, Show)

categoryName :: CryptoCategory -> Text
categoryName CatSymmetricEncryption = "Symmetric Encryption"
categoryName CatHashFunctions = "Hash Functions"
categoryName CatMacs = "MACs"
categoryName CatAsymmetricSignatures = "Asymmetric / Signatures"
categoryName CatKeyExchange = "Key Exchange"
categoryName CatKdfs = "KDFs / Password Hashing"
categoryName CatOther = "Other"

classifyPrimitive :: CryptoPrimitive -> CryptoCategory
classifyPrimitive PrimitiveAe = CatSymmetricEncryption
classifyPrimitive PrimitiveBlockCipher = CatSymmetricEncryption
classifyPrimitive PrimitiveStreamCipher = CatSymmetricEncryption
classifyPrimitive PrimitiveHash = CatHashFunctions
classifyPrimitive PrimitiveXof = CatHashFunctions
classifyPrimitive PrimitiveMac = CatMacs
classifyPrimitive PrimitiveSignature = CatAsymmetricSignatures
classifyPrimitive PrimitivePke = CatAsymmetricSignatures
classifyPrimitive PrimitiveKem = CatKeyExchange
classifyPrimitive PrimitiveKeyAgree = CatKeyExchange
classifyPrimitive PrimitiveKeyWrap = CatSymmetricEncryption
classifyPrimitive PrimitiveKdf = CatKdfs
classifyPrimitive PrimitiveDrbg = CatOther
classifyPrimitive PrimitiveCombiner = CatOther
classifyPrimitive PrimitiveOther = CatOther
classifyPrimitive PrimitiveUnknown = CatOther

categorizeFindings :: [CryptoFinding] -> Map CryptoCategory [CryptoFinding]
categorizeFindings = foldr categorize Map.empty
  where
    categorize finding acc =
      let cat = classifyPrimitive (cryptoAlgorithmPrimitive $ cryptoFindingAlgorithm finding)
       in Map.insertWith (++) cat [finding] acc

renderCategoryBreakdown :: Map CryptoCategory [CryptoFinding] -> Doc AnsiStyle
renderCategoryBreakdown categorized =
  vcat $
    [annotate bold "Per-Category Breakdown", annotate bold "----------------------"]
      ++ concatMap renderCategory (Map.toAscList categorized)

renderCategory :: (CryptoCategory, [CryptoFinding]) -> [Doc AnsiStyle]
renderCategory (cat, fs) =
  [ ""
  , annotate bold $ pretty (categoryName cat) <> ":"
  ]
    ++ map (indent 2 . renderAlgoStatus) fs

renderAlgoStatus :: CryptoFinding -> Doc AnsiStyle
renderAlgoStatus finding =
  let algo = cryptoFindingAlgorithm finding
      statusDoc = case cryptoAlgorithmFipsStatus algo of
        FipsApproved -> annotate (color Green) "Approved"
        FipsDeprecated -> annotate (color Yellow) "Deprecated"
        FipsNotApproved -> annotate (color Red) "Not Approved"
   in "- "
        <> pretty (cryptoAlgorithmName algo)
        <> " ["
        <> statusDoc
        <> "]"
        <> maybe mempty (\ps -> " (" <> pretty ps <> "-bit)") (cryptoAlgorithmParameterSet algo)

renderRemediationTable :: [CryptoFinding] -> Doc AnsiStyle
renderRemediationTable fs =
  let nonFips = filter (\f -> cryptoAlgorithmFipsStatus (cryptoFindingAlgorithm f) == FipsNotApproved) fs
   in if null nonFips
        then annotate (color Green) "No remediation needed - all algorithms are FIPS approved or deprecated."
        else
          vcat $
            [ annotate bold "Remediation Recommendations"
            , annotate bold "---------------------------"
            , ""
            , padRight 35 "Non-FIPS Algorithm" <> pretty ("Recommended FIPS Alternative" :: Text)
            , padRight 35 "------------------" <> pretty ("---------------------------" :: Text)
            ]
              ++ map renderRemediation nonFips

renderRemediation :: CryptoFinding -> Doc AnsiStyle
renderRemediation finding =
  let name = cryptoAlgorithmName (cryptoFindingAlgorithm finding)
      alternative = suggestAlternative name
   in padRight 35 name <> pretty alternative

padRight :: Int -> Text -> Doc ann
padRight n t = pretty t <> pretty (Text.replicate (max 0 (n - Text.length t)) " ")

suggestAlternative :: Text -> Text
suggestAlternative name =
  let lower = Text.toLower name
      check = any (\p -> Text.toLower p == lower)
   in if check ["chacha20", "chacha20-poly1305", "xchacha20"]
        then "AES-256-GCM"
        else
          if check ["blake2", "blake2b", "blake2s", "blake3"]
            then "SHA-256 / SHA-3"
            else
              if check ["md5"] || check ["md4"]
                then "SHA-256"
                else
                  if check ["rc4", "rc2", "blowfish", "des", "3des", "3des-encrypt"]
                    then "AES-256"
                    else
                      if check ["bcrypt", "argon2", "argon2i", "argon2id", "scrypt"]
                        then "PBKDF2"
                        else
                          if check ["x25519", "x448"]
                            then "ECDH P-256 / P-384"
                            else
                              if check ["curve25519"]
                                then "ECDH NIST curves"
                                else
                                  if check ["poly1305"]
                                    then "HMAC / CMAC"
                                    else
                                      if check ["siphash"]
                                        then "HMAC"
                                        else
                                          if check ["whirlpool", "ripemd", "ripemd-160"]
                                            then "SHA-256"
                                            else
                                              if check ["cast5", "idea", "camellia", "seed", "aria"]
                                                || check ["twofish", "serpent", "threefish"]
                                                then "AES-256"
                                                else "Review FIPS 140-3 approved algorithm list"

renderKeySizeWarnings :: [CryptoFinding] -> Doc AnsiStyle
renderKeySizeWarnings fs =
  let warnings = concatMap keySizeWarning fs
   in if null warnings
        then mempty
        else
          vcat $
            [ annotate bold "Key Size Warnings"
            , annotate bold "-----------------"
            ]
              ++ warnings
              ++ [hardline]

keySizeWarning :: CryptoFinding -> [Doc AnsiStyle]
keySizeWarning finding =
  let algo = cryptoFindingAlgorithm finding
      algoName = Text.toLower $ cryptoAlgorithmName algo
      paramSet = cryptoAlgorithmParameterSet algo
      prim = cryptoAlgorithmPrimitive algo
      isHash = prim == PrimitiveHash || prim == PrimitiveXof
   in catWarnings algoName paramSet isHash
  where
    catWarnings :: Text -> Maybe Text -> Bool -> [Doc AnsiStyle]
    catWarnings n ps isHashPrim
      | "rsa" `Text.isInfixOf` n = rsaWarning ps
      | isHashPrim && ("sha-1" `Text.isInfixOf` n || "sha1" `Text.isInfixOf` n) =
          [annotate (color Yellow) "- SHA-1: Deprecated, fully disallowed after 2030-12-31"]
      | "aes-128" `Text.isInfixOf` n || (n == "aes" && ps == Just "128") =
          [annotate (color Yellow) "- AES-128: Approved but AES-256 recommended for higher security margin"]
      | isHashPrim && "sha-224" `Text.isInfixOf` n =
          [annotate (color Yellow) "- SHA-224: Deprecated by 2030"]
      | "3des" `Text.isInfixOf` n || "triple-des" `Text.isInfixOf` n =
          [annotate (color Yellow) "- 3DES: Legacy decryption only since Jan 2024"]
      | otherwise = []

    rsaWarning :: Maybe Text -> [Doc AnsiStyle]
    rsaWarning Nothing = [annotate (color Yellow) "- RSA: Key size not detected, ensure >= 2048-bit"]
    rsaWarning (Just ps) =
      case reads (toString ps) :: [(Int, String)] of
        [(n, "")] ->
          if n < 2048
            then [annotate (color Red) $ "- RSA-" <> pretty ps <> ": Below FIPS minimum (2048-bit required)"]
            else
              if n < 3072
                then [annotate (color Yellow) $ "- RSA-" <> pretty ps <> ": Approved but RSA-3072+ recommended for 128-bit security"]
                else []
        _ -> [annotate (color Yellow) $ "- RSA-" <> pretty ps <> ": Key size format not recognized, ensure >= 2048-bit"]
