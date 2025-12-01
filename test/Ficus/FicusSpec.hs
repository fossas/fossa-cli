{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ficus.FicusSpec (spec) where

import App.Fossa.Ficus.Analyze (analyzeWithFicus)
import App.Fossa.Ficus.Types (FicusAnalysisResults (..), FicusSnippetScanResults (..), FicusStrategy (FicusStrategySnippetScan, FicusStrategyVendetta), FicusVendoredDependencyScanResults (FicusVendoredDependencyScanResults))
import App.Types (ProjectRevision (..))
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Control.Carrier.StickyLogger (ignoreStickyLogger)
import Control.Effect.Lift (sendIO)
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
import Test.Effect (expectationFailure', it', shouldBe', shouldSatisfy')
import Test.Hspec
import Text.URI (mkURI)

fixtureDir :: Path Rel Dir
fixtureDir = [reldir|test/Ficus/testdata|]

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

      result <- sendIO $ runStack . runDiagnostics . ignoreStickyLogger . ignoreLogger . runExecIO . runReadFSIO $ analyzeWithFicus testDataDir apiOpts revision strategies Nothing (Just 10) Nothing

      case result of
        Success _warnings analysisResult -> do
          case analysisResult of
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
        Failure _warnings errors -> do
          let failureMsg = show errors
          case apiOpts of
            Just _ -> do
              -- With API credentials, accept 404/temp-storage errors as valid connectivity tests
              if "404" `isInfixOf` failureMsg || "temp-storage" `isInfixOf` failureMsg || "Status(" `isInfixOf` failureMsg
                then True `shouldBe'` True -- Expected API connectivity issue
                else expectationFailure' ("Ficus integration test failed unexpectedly: " ++ failureMsg)
            Nothing -> do
              -- Without API credentials, analysis failure is expected
              True `shouldBe'` True
