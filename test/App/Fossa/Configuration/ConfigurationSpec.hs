{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Configuration.ConfigurationSpec
  ( spec,
  )
where

import App.Fossa.Configuration
import qualified Control.Carrier.Diagnostics as Diag
import Effect.ReadFS
import Path
import qualified Test.Hspec as T

expectedConfigFile :: ConfigFile
expectedConfigFile =
  ConfigFile
    { configVersion = 3,
      configServer = Just "https://app.fossa.com",
      configApiKey = Just "123",
      configProject = Just expectedConfigProject,
      configRevision = Just expectedConfigRevision
    }

expectedConfigProject :: ConfigProject
expectedConfigProject =
  ConfigProject
    { configProjID = Just "github.com/fossa-cli",
      configName = Just "fossa-cli",
      configLink = Just "fossa.com",
      configTeam = Just "fossa-team",
      configJiraKey = Just "key",
      configUrl = Just "fossa.com",
      configPolicy = Just "license-policy"
    }

expectedConfigRevision :: ConfigRevision
expectedConfigRevision =
  ConfigRevision
    { configCommit = Just "12345",
      configBranch = Just "master"
    }

testFile :: Path Rel File
testFile = $(mkRelFile "test/App/Fossa/Configuration/testdata/validconfig.yml")

badFile :: Path Rel File
badFile = $(mkRelFile "test/App/Fossa/Configuration/testdata/invalidconfig.yml")

missingFile :: Path Rel File
missingFile = $(mkRelFile "test/App/Fossa/Configuration/testdata/missingfile.yml")

spec :: T.Spec
spec = do
  config <- T.runIO . Diag.runDiagnostics $ runReadFSIO $ readConfigFile testFile
  badConfig <- T.runIO . Diag.runDiagnostics $ runReadFSIO $ readConfigFile badFile
  missingConfig <- T.runIO . Diag.runDiagnostics $ runReadFSIO $ readConfigFile missingFile

  T.describe "config file parser" $ do
    T.it "parses a full configuration file correctly" $
      case config of
        Left err -> T.expectationFailure ("failed to parse config file" <> show (Diag.renderFailureBundle err))
        Right a -> case a of
          Nothing -> T.expectationFailure "config file was Nothing after parsing"
          Just result -> result `T.shouldBe` expectedConfigFile

    T.it "returns failure for a bad file" $
      case badConfig of
        Left err -> show (Diag.renderFailureBundle err) `T.shouldNotBe` ""
        Right _ -> T.expectationFailure "should have failed parsing"

    T.it "returns Nothing for missing file" $
      case missingConfig of
        Left _ -> T.expectationFailure "should have failed parsing"
        Right result -> result `T.shouldBe` Nothing
