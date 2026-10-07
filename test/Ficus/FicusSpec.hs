{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ficus.FicusSpec (spec) where

import App.Fossa.EmbeddedBinary (BinaryPaths (..))
import App.Fossa.Ficus.Analyze (analyzeWithFicus, ficusCommand, vendoredDepsToSourceUnit)
import App.Fossa.Ficus.Types (FicusAllFlag (..), FicusAnalysisFlag (..), FicusAnalysisResults (..), FicusConfig (..), FicusPerStrategyFlag (..), FicusSnippetScanFlag (..), FicusSnippetScanResults (..), FicusStrategy (FicusStrategySnippetScan, FicusStrategyVendetta), FicusVendoredDependency (..), FicusVendoredDependencyScanResults (FicusVendoredDependencyScanResults), FicusVendoredLocation (..), renderFicusPerStrategyFlag)
import App.Types (ProjectRevision (..))
import Control.Effect.Lift (sendIO)
import Control.Timeout (Duration (Seconds))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Vector qualified
import Effect.Exec (Command (cmdArgs))
import Fossa.API.Types (ApiKey (..), ApiOpts (..))
import Path (Abs, Dir, Path, Rel, reldir, relfile, toFilePath, (</>))
import Path.IO qualified as PIO
import Srclib.Types (SourceUnit (..), SourceUnitBuild (..), SourceUnitDependency (..))
import System.Environment (lookupEnv)
import Test.Effect (expectationFailure', it', shouldBe', shouldSatisfy')
import Test.Hspec
import Text.URI (mkURI)

fixtureDir :: Path Rel Dir
fixtureDir = [reldir|test/Ficus/testdata|]

ficusConfigWith :: Path Abs Dir -> [FicusSnippetScanFlag] -> FicusConfig
ficusConfigWith rootDir snippetScanFlags =
  FicusConfig
    { ficusConfigRootDir = rootDir
    , ficusConfigExclude = []
    , ficusConfigEndpoint = Nothing
    , ficusConfigSecret = Just $ ApiKey "test-key"
    , ficusConfigRevision = ProjectRevision "project" "revision" Nothing
    , ficusConfigFlags = [All $ FicusAllFlag SkipHiddenFiles, All $ FicusAllFlag Gitignore]
    , ficusConfigSnippetScanFlags = snippetScanFlags
    , ficusConfigSnippetScanRetentionDays = Nothing
    , ficusConfigStrategies = [FicusStrategySnippetScan]
    }

-- | The ficus arguments emitted before any snippet-scan flags were supported.
baseArgsBefore :: [Text]
baseArgsBefore =
  [ "analyze"
  , "--secret"
  , "test-key"
  , "--endpoint"
  , "https://app.fossa.com/api/proxy/analysis"
  , "--locator"
  , "custom+project$revision"
  , "--set"
  , "all:skip-hidden-files"
  , "--set"
  , "all:gitignore"
  ]

baseArgsAfter :: Path Abs Dir -> [Text]
baseArgsAfter rootDir =
  [ "--exclude"
  , ".git"
  , "--exclude"
  , ".git/**"
  , "--strategy"
  , "snippet-scanning"
  , toText (toFilePath rootDir)
  ]

spec :: Spec
spec = do
  describe "Ficus snippet scanning integration" $ do
    it' "should run ficus binary successfully" $ do
      -- Check for API configuration from environment
      maybeApiKey <- sendIO $ lookupEnv "FOSSA_API_KEY"
      maybeEndpoint <- sendIO $ lookupEnv "FOSSA_ENDPOINT"

      apiOpts <- case (maybeApiKey, maybeEndpoint) of
        (Just keyStr, Just endpointStr) ->
          case mkURI (toText endpointStr) of
            Just uri -> do
              let opts = ApiOpts (Just uri) (ApiKey (toText keyStr)) (Seconds 60)
              pure (Just opts)
            Nothing -> do
              expectationFailure' $ "Invalid API endpoint URL: " ++ endpointStr
              pure Nothing
        _ -> pure Nothing

      currentDir <- sendIO PIO.getCurrentDir
      let testDataDir = currentDir </> fixtureDir
          revision = ProjectRevision "ficus-integration-test" "testdata-123456" (Just "integration-test")

      -- Check if test data exists
      testDataExists <- sendIO $ PIO.doesDirExist testDataDir
      testDataExists `shouldBe'` True

      let strategies = [FicusStrategySnippetScan, FicusStrategyVendetta]

      result <- analyzeWithFicus testDataDir apiOpts revision strategies [] Nothing (Just 10) Nothing

      case result of
        Just results -> do
          case snippetScanResults results of
            Just snippetResults -> do
              ficusSnippetScanResultsAnalysisId snippetResults `shouldSatisfy'` (> 0)
            _ -> do
              -- No snippet scan results returned - this is acceptable for integration testing
              True `shouldBe'` True

          case vendoredDependencyScanResults results of
            Just (FicusVendoredDependencyScanResults (Just srcUnit)) -> do
              sourceUnitName srcUnit `shouldBe'` "ficus-vendored-dependencies"
            _ -> do
              -- No vendetta results returned - this is acceptable for integration testing
              True `shouldBe'` True
        _ -> expectationFailure' "Ficus analysis returned no results unexpectedly."

  describe "ficusCommand" $ do
    let argsFor flags = do
          rootDir <- sendIO PIO.getCurrentDir
          let bin = BinaryPaths rootDir [relfile|ficus|]
          cmd <- ficusCommand (ficusConfigWith rootDir flags) bin False
          pure (rootDir, cmdArgs cmd)

    it' "emits no snippet-scan flags when none are requested" $ do
      (rootDir, args) <- argsFor []
      args `shouldBe'` (baseArgsBefore <> baseArgsAfter rootDir)

    it' "emits skip-headers when header skipping is enabled" $ do
      (rootDir, args) <- argsFor [SnippetScanSkipHeaders]
      args `shouldBe'` (baseArgsBefore <> ["--set", "snippet-scan:skip-headers"] <> baseArgsAfter rootDir)

    it' "emits skip-headers and its limit when both are requested" $ do
      (rootDir, args) <- argsFor [SnippetScanSkipHeaders, SnippetScanSkipHeadersLimit 20]
      let expected =
            baseArgsBefore
              <> ["--set", "snippet-scan:skip-headers", "--set", "snippet-scan:skip-headers-limit=20"]
              <> baseArgsAfter rootDir
      args `shouldBe'` expected

  describe "renderFicusPerStrategyFlag" $ do
    it "renders flags as ficus parses them" $ do
      renderFicusPerStrategyFlag (All $ FicusAllFlag SkipHiddenFiles) `shouldBe` "all:skip-hidden-files"
      renderFicusPerStrategyFlag (SnippetScan $ SnippetScanCommonFlag AllExtensions) `shouldBe` "snippet-scan:all-extensions"
      renderFicusPerStrategyFlag (SnippetScan $ SnippetScanBatchLen 50) `shouldBe` "snippet-scan:batch-len=50"
      renderFicusPerStrategyFlag (SnippetScan SnippetScanSkipHeaders) `shouldBe` "snippet-scan:skip-headers"
      renderFicusPerStrategyFlag (SnippetScan $ SnippetScanSkipHeadersLimit 0) `shouldBe` "snippet-scan:skip-headers-limit=0"

  describe "vendoredDepsToSourceUnit" $ do
    it "serializes classified locations into vendored metadata" $ do
      let dep =
            FicusVendoredDependency
              { ficusVendoredDependencyName = "github.com/test/dep"
              , ficusVendoredDependencyEcosystem = "git"
              , ficusVendoredDependencyVersion = Nothing
              , ficusVendoredDependencyLocations =
                  [ FicusVendoredDirectory "vendored"
                  , FicusVendoredFile "sqlite3.c"
                  ]
              }

      let result = vendoredDepsToSourceUnit [dep]

      sourceUnitName result `shouldBe` "ficus-vendored-dependencies"

      -- Check origin paths contain all locations
      length (sourceUnitOriginPaths result) `shouldBe` 2

      -- Check the vendored metadata preserves the type/path classification
      case sourceUnitBuild result of
        Nothing -> expectationFailure "Expected SourceUnitBuild"
        Just build -> case buildDependencies build of
          [srcDep] -> case sourceDepData srcDep of
            Aeson.Object obj -> case KeyMap.lookup (Key.fromString "vendored") obj of
              Just (Aeson.Array entries) -> do
                length entries `shouldBe` 2
                -- First entry should be the directory
                case entries Data.Vector.!? 0 of
                  Just (Aeson.Object e) -> do
                    KeyMap.lookup (Key.fromString "type") e `shouldBe` Just (Aeson.String "directory")
                    KeyMap.lookup (Key.fromString "path") e `shouldBe` Just (Aeson.String "vendored")
                  _ -> expectationFailure "Expected object in vendored array"
                -- Second entry should be the file
                case entries Data.Vector.!? 1 of
                  Just (Aeson.Object e) -> do
                    KeyMap.lookup (Key.fromString "type") e `shouldBe` Just (Aeson.String "file")
                    KeyMap.lookup (Key.fromString "path") e `shouldBe` Just (Aeson.String "sqlite3.c")
                  _ -> expectationFailure "Expected object in vendored array"
              _ -> expectationFailure "Expected 'vendored' array in metadata"
            _ -> expectationFailure "Expected object in sourceDepData"
          _ -> expectationFailure "Expected exactly one dependency"
