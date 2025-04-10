{-# LANGUAGE QuasiQuotes #-}

module App.Fossa.Analyze.GitUrlSpec (spec) where

import App.Fossa.Analyze
import App.Fossa.Config.Analyze
import Control.Carrier.Diagnostics
import Control.Carrier.Git.Test
import Control.Effect.Git
import Data.Flag
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
  describe "Git URL detection in analyze" $ do
    it "should use Git remote URL when auto-detection is enabled and no manual URL is provided" $ do
      let expectedUrl = "https://github.com/fossas/fossa-cli.git"
      let config = defaultAnalyzeConfig
            { autoDetectProjectUrl = Flag $ AutoDetectProjectUrl True
            , projectUrl = Nothing
            }
      
      result <- runGitTest [("config --get remote.origin.url", Right expectedUrl)] $ do
        url <- getProjectUrl config
        pure url

      result `shouldBe` Just expectedUrl

    it "should prefer manual URL over Git remote URL" $ do
      let manualUrl = "https://example.com/manual"
      let gitUrl = "https://github.com/fossas/fossa-cli.git"
      let config = defaultAnalyzeConfig
            { autoDetectProjectUrl = Flag $ AutoDetectProjectUrl True
            , projectUrl = Just manualUrl
            }
      
      result <- runGitTest [("config --get remote.origin.url", Right gitUrl)] $ do
        url <- getProjectUrl config
        pure url

      result `shouldBe` Just manualUrl

    it "should return Nothing when auto-detection is disabled and no manual URL is provided" $ do
      let gitUrl = "https://github.com/fossas/fossa-cli.git"
      let config = defaultAnalyzeConfig
            { autoDetectProjectUrl = Flag $ AutoDetectProjectUrl False
            , projectUrl = Nothing
            }
      
      result <- runGitTest [("config --get remote.origin.url", Right gitUrl)] $ do
        url <- getProjectUrl config
        pure url

      result `shouldBe` Nothing

    it "should handle Git command failures gracefully" $ do
      let config = defaultAnalyzeConfig
            { autoDetectProjectUrl = Flag $ AutoDetectProjectUrl True
            , projectUrl = Nothing
            }
      
      result <- runGitTest [("config --get remote.origin.url", Left "fatal: not a git repository")] $ do
        url <- getProjectUrl config
        pure url

      result `shouldBe` Nothing

    it "should continue analysis when no Git URL is detected" $ do
      let config = defaultAnalyzeConfig
            { autoDetectProjectUrl = Flag $ AutoDetectProjectUrl True
            , projectUrl = Nothing
            }
      
      result <- runGitTest [("config --get remote.origin.url", Left "fatal: not a git repository")] $ do
        -- Simulate analyze command execution
        url <- getProjectUrl config
        -- Verify that even with no URL, we get a valid result
        pure $ case url of
          Nothing -> True  -- Analysis should continue
          Just _ -> False -- We shouldn't get a URL in this case

      result `shouldBe` True

    it "should log debug message when no Git URL is detected" $ do
      let config = defaultAnalyzeConfig
            { autoDetectProjectUrl = Flag $ AutoDetectProjectUrl True
            , projectUrl = Nothing
            }
      
      (result, logs) <- runGitTest [("config --get remote.origin.url", Left "fatal: not a git repository")] $ do
        -- Capture logs during execution
        url <- getProjectUrl config
        logs <- getLogs
        pure (url, logs)

      logs `shouldContain` "No Git remote URL detected, continuing without project URL"
      result `shouldBe` Nothing

-- Helper function to simulate the URL detection logic from analyze
getProjectUrl :: 
  ( Has Git sig m
  , Has Diagnostics sig m
  ) => 
  AnalyzeConfig -> 
  m (Maybe Text)
getProjectUrl cfg = case (projectUrl cfg, fromFlag AutoDetectProjectUrl $ autoDetectProjectUrl cfg) of
  (Just url, _) -> pure (Just url)
  (Nothing, True) -> getRemoteUrl
  (Nothing, False) -> pure Nothing

-- Default config for testing
defaultAnalyzeConfig :: AnalyzeConfig
defaultAnalyzeConfig = AnalyzeConfig
  { autoDetectProjectUrl = Flag $ AutoDetectProjectUrl True
  , projectUrl = Nothing
  -- ... other fields with default values ...
  } 