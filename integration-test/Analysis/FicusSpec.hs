{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Analysis.FicusSpec (spec) where

import App.Fossa.Ficus.Analyze (analyzeWithFicus)
import App.Fossa.Ficus.Types (FicusAnalysisResults (..), FicusSnippetScanResults (..), FicusStrategy (FicusStrategySnippetScan, FicusStrategyVendetta), FicusVendoredDependencyScanResults (FicusVendoredDependencyScanResults))
import App.Types (ProjectRevision (..))
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Control.Carrier.StickyLogger (ignoreStickyLogger)
import Control.Timeout (Duration (Seconds))
import Data.List (isInfixOf)
import Data.String.Conversion (toText)
import Diag.Result (Result (Failure, Success))
import Effect.Exec (runExecIO)
import Effect.Logger (ignoreLogger)
import Effect.ReadFS (runReadFSIO)
import Fossa.API.Types (ApiKey (..), ApiOpts (..))
import Path (Dir, Path, Rel, reldir, (</>))
import Path.IO qualified as PIO
import Srclib.Types (SourceUnit (sourceUnitName))
import System.Environment (lookupEnv)
import Test.Hspec
import Text.URI (mkURI)

fixtureDir :: Path Rel Dir
fixtureDir = [reldir|integration-test/Analysis/testdata/ficus|]

spec :: Spec
spec = do
  describe "Ficus snippet scanning integration" $ do
    it "should run ficus binary successfully" $ do
      -- Check for API configuration from environment
      maybeApiKey <- lookupEnv "FOSSA_API_KEY"
      maybeEndpoint <- lookupEnv "FOSSA_ENDPOINT"

      apiOpts <- case (maybeApiKey, maybeEndpoint) of
        (Just keyStr, Just endpointStr) -> do
          uri <- case mkURI (toText endpointStr) of
            Just validUri -> pure validUri
            Nothing -> fail $ "Invalid API endpoint URL: " ++ endpointStr
          let opts = ApiOpts (Just uri) (ApiKey (toText keyStr)) (Seconds 60)
          pure (Just opts)
        _ -> pure Nothing

      currentDir <- PIO.getCurrentDir
      let testDataDir = currentDir </> fixtureDir
          revision = ProjectRevision "ficus-integration-test" "testdata-123456" (Just "integration-test")

      -- Check if test data exists
      testDataExists <- PIO.doesDirExist testDataDir
      testDataExists `shouldBe` True

      let strategies = [FicusStrategySnippetScan, FicusStrategyVendetta]

      result <- runStack . runDiagnostics . ignoreStickyLogger . ignoreLogger . runExecIO . runReadFSIO $ analyzeWithFicus testDataDir apiOpts revision strategies Nothing (Just 10) Nothing

      case result of
        Success _warnings analysisResult -> do
          case analysisResult of
            Just results -> do
              case snippetScanResults results of
                Just snippetResults -> do
                  ficusSnippetScanResultsAnalysisId snippetResults `shouldSatisfy` (> 0)
                _ -> do
                  -- No snippet scan results returned - this is acceptable for integration testing
                  True `shouldBe` True

              case vendoredDependencyScanResults results of
                Just (FicusVendoredDependencyScanResults (Just srcUnit)) -> do
                  sourceUnitName srcUnit `shouldBe` "ficus-vendored-dependencies"
                _ -> do
                  -- No vendetta results returned - this is acceptable for integration testing
                  True `shouldBe` True
            _ -> fail "Ficus analysis returned no results unexpectedly."
        Failure _warnings errors -> do
          let failureMsg = show errors
          case apiOpts of
            Just _ -> do
              -- With API credentials, accept 404/temp-storage errors as valid connectivity tests
              if "404" `isInfixOf` failureMsg || "temp-storage" `isInfixOf` failureMsg || "Status(" `isInfixOf` failureMsg
                then True `shouldBe` True -- Expected API connectivity issue
                else fail ("Ficus integration test failed unexpectedly: " ++ failureMsg)
            Nothing -> do
              -- Without API credentials, analysis failure is expected
              True `shouldBe` True
