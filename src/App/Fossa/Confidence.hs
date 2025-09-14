{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Confidence (
  ConfidenceScore (..),
  Finding (..),
  ValidationFactor (..),
  FactorType (..),
  runConfidenceAnalysis,
  confidenceValue,
) where

import Control.Effect.Lift (Has, Lift, sendIO)
import Data.Text (Text)
import Data.Text qualified as T
import Effect.Logger (Logger, logInfo)
import Prettyprinter (pretty)
import Fossa.API.Types (Issue (..), Issues (..))
import Srclib.Types (parseLocator, locatorProject, locatorFetcher)
import System.Directory (doesDirectoryExist, listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension, takeFileName)
import System.Process (readProcess)
import Control.Exception (try, SomeException)
import Control.Monad (forM)
import Data.Text.IO qualified as TIO
import Data.List (isPrefixOf)
import Data.Text (isInfixOf)

-- | Confidence score from 0-100
newtype ConfidenceScore = ConfidenceScore Int
  deriving newtype (Show, Eq, Ord)

-- | Types of validation factors
data FactorType
  = ManifestConsistency
  | BinaryPresence
  | SourceCodeUsage
  deriving stock (Show, Eq)

-- | A single validation check result
data ValidationFactor = ValidationFactor
  { factorType :: FactorType,
    factorPassed :: Bool,
    factorWeight :: Int,
    factorDescription :: Text
  }
  deriving stock (Show)

-- | A finding with confidence analysis
data Finding = Finding
  { findingIssue :: Issue,
    findingConfidence :: ConfidenceScore,
    findingFactors :: [ValidationFactor],
    findingSuppressed :: Bool
  }
  deriving stock (Show)

-- | The main entry point for running confidence analysis on a set of issues.
runConfidenceAnalysis ::
  (Has (Lift IO) sig m, Has Logger sig m) =>
  FilePath ->
  Text ->
  Text ->
  Int ->
  Issues ->
  m Issues
runConfidenceAnalysis projectDir rev1 rev2 threshold issues =
  if null (issuesIssues issues)
    then pure issues
    else do
      logInfo "\n=== FOSSA Confidence Analysis Engaged ==="
      let analyze = analyzeSingleFinding projectDir rev1 rev2 threshold
      findings <- traverse analyze (issuesIssues issues)

      let highConfidenceFindings = filter (not . findingSuppressed) findings
          suppressedCount = length findings - length highConfidenceFindings
          highConfidenceIssues = findingIssue <$> highConfidenceFindings

      logInfo $ pretty $ ("Suppressed " <> T.pack (show suppressedCount) <> " low-confidence issues." :: Text)
      logInfo $ pretty ("=========================================" :: Text)

      pure $ issues {issuesIssues = highConfidenceIssues, issuesCount = length highConfidenceIssues}

-- | Analyze a single issue and produce a Finding with a confidence score.
analyzeSingleFinding ::
  (Has (Lift IO) sig m) =>
  FilePath ->
  Text ->
  Text ->
  Int ->
  Issue ->
  m Finding
analyzeSingleFinding projectDir rev1 rev2 threshold issue = do
  -- This is where the real work happens. We gather evidence.
  factors <- gatherValidationFactors projectDir rev1 rev2 issue
  let confidence = calculateConfidence factors
      suppressed = confidenceValue confidence < threshold

  pure
    Finding
      { findingIssue = issue,
        findingConfidence = confidence,
        findingFactors = factors,
        findingSuppressed = suppressed
      }

-- | Gathers all evidence for a given issue.
-- In a real implementation, this would involve filesystem access, git history, etc.
-- For this PoC, we are using placeholders.
gatherValidationFactors :: (Has (Lift IO) sig m) => FilePath -> Text -> Text -> Issue -> m [ValidationFactor]
gatherValidationFactors projectDir rev1 rev2 issue = do
  -- These are placeholders! You would replace `sendIO $ pure ...` with real checks.
  manifestFactor <- sendIO $ checkManifestConsistency projectDir rev1 rev2 issue
  binaryFactor <- sendIO $ checkBinaryPresence projectDir issue
  sourceFactor <- sendIO $ checkSourceCodeUsage projectDir issue
  pure [manifestFactor, binaryFactor, sourceFactor]

-- | Check if the change appears to be only a version bump by inspecting the git diff of the manifest file.
checkManifestConsistency :: FilePath -> Text -> Text -> Issue -> IO ValidationFactor
checkManifestConsistency projectDir rev1 rev2 issue = do
  if rev1 == rev2
    then
      pure $
        ValidationFactor
          { factorType = ManifestConsistency,
            factorPassed = True, -- Not a diff, so we don't penalize
            factorWeight = 30,
            factorDescription = "Not a diff comparison, skipping consistency check."
          }
    else do
      let locator = parseLocator (issueRevisionId issue)
          fetcher = locatorFetcher locator
          packageName = locatorProject locator

      -- Map fetcher to manifest file, focusing on common ones for this PoC
      let manifestFile = case fetcher of
            "npm" -> Just "package.json"
            "gomod" -> Just "go.mod"
            _ -> Nothing

      case manifestFile of
        Nothing ->
          pure $
            ValidationFactor
              { factorType = ManifestConsistency,
                factorPassed = True, -- Not applicable, so we don't penalize
                factorWeight = 30,
                factorDescription = "Manifest consistency check not applicable for fetcher: " <> fetcher
              }
        Just manifest -> do
          let manifestPath = projectDir </> manifest
          exists <- doesFileExist manifestPath
          if not exists
            then
              pure $
                ValidationFactor
                  { factorType = ManifestConsistency,
                    factorPassed = True, -- File not found, can't check, don't penalize
                    factorWeight = 30,
                    factorDescription = "Manifest file not found: " <> T.pack manifest
                  }
            else do
              -- Run git diff and handle potential errors
              diffResult <- try (readProcess "git" ["diff", T.unpack rev1, T.unpack rev2, "--", manifest] "") :: IO (Either SomeException String)
              case diffResult of
                Left e ->
                  pure $
                    ValidationFactor
                      { factorType = ManifestConsistency,
                        factorPassed = True, -- Error running git, don't penalize
                        factorWeight = 30,
                        factorDescription = "Error running git diff: " <> T.pack (show e)
                      }
                Right diff -> do
                  -- Heuristic: if a line containing the package name was both added and removed, it's likely a version bump.
                  let diffLines = T.lines (T.pack diff)
                      relevantLines = filter (\l -> not (T.isPrefixOf "---" l) && not (T.isPrefixOf "+++" l)) diffLines
                      packageLines = filter (isInfixOf packageName) relevantLines
                      addedLines = filter (T.isPrefixOf "+") packageLines
                      removedLines = filter (T.isPrefixOf "-") packageLines
                      -- This is a simple heuristic: one line added, one removed, for the same package.
                      isVersionBump = length addedLines == 1 && length removedLines == 1
                      -- factorPassed is False if it's just a version bump.
                      passed = not isVersionBump
                      desc = if passed then "Change appears to be more than a version bump" else "Appears to be version bump only"
                  pure $ ValidationFactor ManifestConsistency passed 30 desc

-- | Check if the dependency is present in build artifacts (e.g., node_modules).
checkBinaryPresence :: FilePath -> Issue -> IO ValidationFactor
checkBinaryPresence projectDir issue = do
  let locator = parseLocator (issueRevisionId issue)
      packageName = T.unpack $ locatorProject locator
      -- This check is specific to node_modules for the PoC.
      dependencyPath = projectDir </> "node_modules" </> packageName

  result <- try (doesDirectoryExist dependencyPath) :: IO (Either SomeException Bool)

  let (passed, desc) = case result of
        Right True -> (True, "Build artifacts found in node_modules")
        Right False -> (False, "Build artifacts not found in node_modules")
        Left e -> (False, "Error checking for build artifacts: " <> T.pack (show e))

  pure $
    ValidationFactor
      { factorType = BinaryPresence,
        factorPassed = passed,
        factorWeight = 25,
        factorDescription = desc
      }

-- | Check if the dependency is actively used in source code by searching for require/import statements.
checkSourceCodeUsage :: FilePath -> Issue -> IO ValidationFactor
checkSourceCodeUsage projectDir issue = do
  let locator = parseLocator (issueRevisionId issue)
      packageName = locatorProject locator
      -- CommonJS and ES Module import patterns
      patterns =
        [ "require('" <> packageName <> "')",
          "require(\"" <> packageName <> "\")",
          "from '" <> packageName <> "'",
          "from \"" <> packageName <> "\""
        ]

  -- Search for usage in .js and .ts files
  allFiles <- getAllSourceFiles projectDir
  usageFound <- anyFileContainsPatterns allFiles patterns

  let (passed, desc) =
        if usageFound
          then (True, "Active usage found in source code")
          else (False, "No active usage found in source code")

  pure $
    ValidationFactor
      { factorType = SourceCodeUsage,
        factorPassed = passed,
        factorWeight = 45,
        factorDescription = desc
      }

-- | Recursively get all source files, avoiding node_modules and dot-directories.
getAllSourceFiles :: FilePath -> IO [FilePath]
getAllSourceFiles dir = go dir
  where
    go currentPath = do
      isDir <- doesDirectoryExist currentPath
      if isDir
        then do
          let baseName = takeFileName currentPath
          if baseName == "node_modules" || "." `isPrefixOf` baseName
            then pure []
            else do
              contents <- listDirectory currentPath
              concat <$> forM contents (go . (currentPath </>))
        else do
          isFile <- doesFileExist currentPath
          let ext = takeExtension currentPath
          if isFile && (ext == ".js" || ext == ".ts")
            then pure [currentPath]
            else pure []

-- | Check if any of a list of files contains any of a list of patterns.
anyFileContainsPatterns :: [FilePath] -> [Text] -> IO Bool
anyFileContainsPatterns files patterns = anyM checkFile files
  where
    checkFile file = do
      result <- try (TIO.readFile file) :: IO (Either SomeException Text)
      case result of
        Left _ -> pure False -- Ignore files that can't be read
        Right content -> pure $ any (`isInfixOf` content) patterns

-- | Monadic version of 'any'.
anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = pure False
anyM p (x : xs) = do
  found <- p x
  if found
    then pure True
    else anyM p xs

-- | Calculate a weighted confidence score based on passed validation factors.
-- This function is PURE. It's easy to unit test.
calculateConfidence :: [ValidationFactor] -> ConfidenceScore
calculateConfidence factors =
  let totalWeight = sum (factorWeight <$> factors)
      passedWeight = sum (factorWeight <$> filter factorPassed factors)
      percentage =
        if totalWeight > 0
          then (passedWeight * 100) `div` totalWeight
          else 0 -- Default to 0 if no factors exist
   in ConfidenceScore percentage

-- | Helper to extract the integer value from the ConfidenceScore newtype.
confidenceValue :: ConfidenceScore -> Int
confidenceValue (ConfidenceScore x) = x