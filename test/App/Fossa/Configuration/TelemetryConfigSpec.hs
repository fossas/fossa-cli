{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Configuration.TelemetryConfigSpec (
  spec,
) where

import App.Fossa.Config.Common (
  CommonOpts (..),
  collectTelemetrySink,
 )
import App.Fossa.Config.ConfigFile (
  ConfigFile (..),
  ConfigTelemetry (ConfigTelemetry),
  ConfigTelemetryScope (FullTelemetry, NoTelemetry),
  OrgWideCustomLicenseConfigPolicy (..),
 )
import App.Fossa.Config.EnvironmentVars (EnvVars (..))
import App.Fossa.DebugDir (newDebugDirRef)
import Control.Effect.Lift (sendIO)
import Control.Carrier.Telemetry.Sink.Common (
  TelemetrySink (
    TelemetrySinkToEndpoint,
    TelemetrySinkToFile
  ),
 )
import Data.Text (Text)
import Fossa.API.Types (
  ApiKey (ApiKey),
  ApiOpts (ApiOpts),
  defaultApiPollDelay,
 )
import Path (Abs, File, Path, mkAbsFile)
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe)

defaultEnvVars :: EnvVars
defaultEnvVars =
  EnvVars
    { envApiKey = Nothing
    , envConfigDebug = False
    , envTelemetryDebug = False
    , envTelemetryScope = Nothing
    , envDockerHost = Nothing
    , envCmdOverrides = mempty
    }

defaultCommonOpts :: CommonOpts
defaultCommonOpts =
  CommonOpts
    { optDebug = False
    , optBaseUrl = Nothing
    , optProjectName = Nothing
    , optProjectRevision = Nothing
    , optAPIKey = Nothing
    , optConfig = Nothing
    , optTelemetry = Nothing
    }

configPath :: Path Abs File
#ifdef mingw32_HOST_OS
configPath = $(mkAbsFile "C:/.fossa.yml")
#else
configPath = $(mkAbsFile "/tmp/.fossa.yml")
#endif

defaultConfigFile :: ConfigFile
defaultConfigFile =
  ConfigFile
    { configVersion = 3
    , configServer = Nothing
    , configApiKey = Just mockApiKeyRaw
    , configReleaseGroup = Nothing
    , configProject = Nothing
    , configRevision = Nothing
    , configTargets = Nothing
    , configPaths = Nothing
    , configExperimental = Nothing
    , configMavenScope = Nothing
    , configVendoredDependencies = Nothing
    , configTelemetry = Nothing
    , configCustomLicenseSearch = Nothing
    , configKeywordSearch = Nothing
    , configOrgWideCustomLicenseConfigPolicy = Use
    , configConfigFilePath = configPath
    , configReachability = Nothing
    }

mockApiKeyRaw :: Text
mockApiKeyRaw = "mockTelemetryApiKey"

mockApiKey :: ApiKey
mockApiKey = ApiKey mockApiKeyRaw

noConfig :: Maybe ConfigFile
noConfig = Nothing

noOpts :: Maybe CommonOpts
noOpts = Nothing

spec :: Spec
spec = do
  describe "telemetry configuration" $ do
    -- This needs to be updated when default telemetry model moves to opt-out.
    it' "by default telemetry sink set to nothing, if api key is not provided" $ do
      debugDirRef <- sendIO newDebugDirRef
      sink <- collectTelemetrySink debugDirRef noConfig defaultEnvVars noOpts
      sink `shouldBe'` Nothing

    it' "by default telemetry sink set to full telemetry" $ do
      debugDirRef <- sendIO newDebugDirRef
      sink <- collectTelemetrySink debugDirRef noConfig defaultEnvVars (Just defaultCommonOpts{optAPIKey = Just mockApiKeyRaw})
      sink `shouldBe'` Just (TelemetrySinkToEndpoint (ApiOpts Nothing mockApiKey defaultApiPollDelay))

    describe "command opts" $ do
      it' "should set sink to nothing, when off scope is provided via command opts" $ do
        debugDirRef <- sendIO newDebugDirRef
        sink <-
          collectTelemetrySink
            debugDirRef
            noConfig
            defaultEnvVars
            (Just defaultCommonOpts{optTelemetry = Just NoTelemetry, optAPIKey = Just mockApiKeyRaw})
        sink `shouldBe'` Nothing

      it' "should set sink to endpoint, when full scope is provided via command opts" $ do
        debugDirRef <- sendIO newDebugDirRef
        sink <-
          collectTelemetrySink
            debugDirRef
            noConfig
            defaultEnvVars
            (Just defaultCommonOpts{optTelemetry = Just FullTelemetry, optAPIKey = Just mockApiKeyRaw})
        sink `shouldBe'` Just (TelemetrySinkToEndpoint (ApiOpts Nothing mockApiKey defaultApiPollDelay))

      it' "should set sink to file, when debug option is provided via command line" $ do
        debugDirRef <- sendIO newDebugDirRef
        telFull <-
          collectTelemetrySink
            debugDirRef
            noConfig
            defaultEnvVars
            (Just defaultCommonOpts{optTelemetry = Just FullTelemetry, optDebug = True, optAPIKey = Just mockApiKeyRaw})
        telFull `shouldBe'` Just (TelemetrySinkToFile debugDirRef)

        telOff <-
          collectTelemetrySink
            debugDirRef
            noConfig
            defaultEnvVars
            (Just defaultCommonOpts{optTelemetry = Just FullTelemetry, optDebug = True, optAPIKey = Just mockApiKeyRaw})
        telOff `shouldBe'` Just (TelemetrySinkToFile debugDirRef)

    describe "environment variables" $ do
      it' "should set sink to nothing, when off scope is provided via environment variables" $ do
        debugDirRef <- sendIO newDebugDirRef
        sink <-
          collectTelemetrySink
            debugDirRef
            noConfig
            defaultEnvVars{envTelemetryScope = Just NoTelemetry, envApiKey = Just mockApiKeyRaw}
            noOpts
        sink `shouldBe'` Nothing

      it' "should set sink to endpoint, when full scope is provided via environment variables" $ do
        debugDirRef <- sendIO newDebugDirRef
        sink <-
          collectTelemetrySink
            debugDirRef
            noConfig
            defaultEnvVars{envTelemetryScope = Just FullTelemetry, envApiKey = Just mockApiKeyRaw}
            noOpts
        sink `shouldBe'` Just (TelemetrySinkToEndpoint (ApiOpts Nothing mockApiKey defaultApiPollDelay))

      it' "should set sink to file, when debug option is provided via environment variable" $ do
        debugDirRef <- sendIO newDebugDirRef
        telFull <-
          collectTelemetrySink
            debugDirRef
            noConfig
            defaultEnvVars
              { envTelemetryScope = Just FullTelemetry
              , envApiKey = Just mockApiKeyRaw
              , envTelemetryDebug = True
              }
            noOpts
        telFull `shouldBe'` Just (TelemetrySinkToFile debugDirRef)

        telOff <-
          collectTelemetrySink
            debugDirRef
            noConfig
            defaultEnvVars
              { envTelemetryScope = Just NoTelemetry
              , envApiKey = Just mockApiKeyRaw
              , envTelemetryDebug = True
              }
            noOpts
        telOff `shouldBe'` Nothing

    describe "configuration file" $ do
      it' "should set sink to nothing, when off scope is provided via configuration file" $ do
        debugDirRef <- sendIO newDebugDirRef
        sink <-
          collectTelemetrySink
            debugDirRef
            (Just defaultConfigFile{configTelemetry = Just $ ConfigTelemetry NoTelemetry})
            defaultEnvVars
            noOpts
        sink `shouldBe'` Nothing

      it' "should set sink to endpoint, when full scope is provided via configuration file" $ do
        debugDirRef <- sendIO newDebugDirRef
        sink <-
          collectTelemetrySink
            debugDirRef
            (Just defaultConfigFile{configTelemetry = Just $ ConfigTelemetry FullTelemetry})
            defaultEnvVars
            noOpts
        sink `shouldBe'` Just (TelemetrySinkToEndpoint (ApiOpts Nothing mockApiKey defaultApiPollDelay))
