{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Config.AnalyzeSpec (spec) where

import App.Fossa.Config.Analyze (
  AnalyzeConfig (filterSet),
  cliParser,
  loadConfig,
  mergeOpts,
 )
import App.Fossa.Config.ConfigFile (ConfigFile (..), ConfigTargets (..))
import App.Fossa.Config.EnvironmentVars (EnvVars (..))
import App.Fossa.Config.Utils (itShouldFailWhenLabelsExceedFive, itShouldLoadFromTheConfiguredBaseDir, parseArgString)
import App.Fossa.Lernie.Types (OrgWideCustomLicenseConfigPolicy (..))
import Data.Text (Text)
import Discovery.Filters (AllFilters (..), combinedTargets)
import Path (mkAbsFile)
import Test.Effect (expectationFailure', it', shouldBe')
import Test.Hspec (Spec, describe)
import Types (TargetFilter (TypeTarget))

envVars :: EnvVars
envVars =
  EnvVars
    { envApiKey = Just "aoeu"
    , envConfigDebug = False
    , envTelemetryDebug = False
    , envTelemetryScope = Nothing
    , envDockerHost = Nothing
    , envCmdOverrides = mempty
    }

configFileWithTargets :: [Text] -> [Text] -> Bool -> ConfigFile
configFileWithTargets only exclude excludeManifestStrategies =
  ConfigFile
    { configVersion = 3
    , configServer = Nothing
    , configApiKey = Nothing
    , configReleaseGroup = Nothing
    , configProject = Nothing
    , configRevision = Nothing
    , configTargets =
        Just $
          ConfigTargets (map TypeTarget only) (map TypeTarget exclude) excludeManifestStrategies
    , configPaths = Nothing
    , configExperimental = Nothing
    , configMavenScope = Nothing
    , configVendoredDependencies = Nothing
    , configTelemetry = Nothing
    , configCustomLicenseSearch = Nothing
    , configKeywordSearch = Nothing
    , configReachability = Nothing
    , configOrgWideCustomLicenseConfigPolicy = Use
    , configConfigFilePath = $(mkAbsFile "/test/fake.yml")
    }

numberOfStrategies :: Int
numberOfStrategies = 47

spec :: Spec
spec = do
  describe "loadConfig" $ do
    itShouldLoadFromTheConfiguredBaseDir cliParser loadConfig

  describe "5 labels are the max" $
    itShouldFailWhenLabelsExceedFive cliParser

  describe "target filters" $ do
    describe "only CLI options" $ do
      it' "should set correct filters when --exclude-manifest-strategies is set" $ do
        let cfgFile = Nothing
        cliOpts <- parseArgString cliParser "--exclude-manifest-strategies"
        filters <- filterSet <$> mergeOpts cfgFile envVars cliOpts
        case (combinedTargets $ includeFilters filters, combinedTargets $ excludeFilters filters) of
          ([], excludedTargets) -> length excludedTargets `shouldBe'` numberOfStrategies
          _ -> expectationFailure' ("Incorrect filters applied. Got " ++ show filters)

      it' "should set correct filters when only filter is set" $ do
        let cfgFile = Nothing
        cliOpts <- parseArgString cliParser "--only-target npm"
        filters <- filterSet <$> mergeOpts cfgFile envVars cliOpts
        case (combinedTargets $ includeFilters filters, combinedTargets $ excludeFilters filters) of
          (includedTargets, []) -> includedTargets `shouldBe'` [TypeTarget "npm"]
          _ -> expectationFailure' ("Incorrect filters applied. Got " ++ show filters)

      it' "should have --exclude-manifest-strategies override only/exclude filters" $ do
        let cfgFile = Nothing
        cliOpts <- parseArgString cliParser "--exclude-manifest-strategies --only-target npm"
        filters <- filterSet <$> mergeOpts cfgFile envVars cliOpts
        case (combinedTargets $ includeFilters filters, combinedTargets $ excludeFilters filters) of
          ([], excludedTargets) -> length excludedTargets `shouldBe'` numberOfStrategies
          _ -> expectationFailure' ("Incorrect filters applied. Got " ++ show filters)

    describe "only config file" $ do
      it' "should set correct filters when targets.excludeManifestStrategies is set" $ do
        let cfgFile = Just $ configFileWithTargets [] [] True
        cliOpts <- parseArgString cliParser ""
        filters <- filterSet <$> mergeOpts cfgFile envVars cliOpts
        case (combinedTargets $ includeFilters filters, combinedTargets $ excludeFilters filters) of
          ([], excludedTargets) -> length excludedTargets `shouldBe'` numberOfStrategies
          _ -> expectationFailure' ("Incorrect filters applied. Got " ++ show filters)

      it' "should set correct filters when targets.only is set" $ do
        let cfgFile = Just $ configFileWithTargets ["npm"] [] False
        cliOpts <- parseArgString cliParser ""
        filters <- filterSet <$> mergeOpts cfgFile envVars cliOpts
        case (combinedTargets $ includeFilters filters, combinedTargets $ excludeFilters filters) of
          (includedTargets, []) -> includedTargets `shouldBe'` [TypeTarget "npm"]
          _ -> expectationFailure' ("Incorrect filters applied. Got " ++ show filters)

      it' "should have targets.excludeManifestStrategies override only/exclude filters" $ do
        let cfgFile = Just $ configFileWithTargets ["npm"] [] True
        cliOpts <- parseArgString cliParser ""
        filters <- filterSet <$> mergeOpts cfgFile envVars cliOpts
        case (combinedTargets $ includeFilters filters, combinedTargets $ excludeFilters filters) of
          ([], excludedTargets) -> length excludedTargets `shouldBe'` numberOfStrategies
          _ -> expectationFailure' ("Incorrect filters applied. Got " ++ show filters)

    describe "config file and CLI options" $ do
      it' "should ignore config file specifying targets.excludeManifestStrategies and just use CLI options" $ do
        let cfgFile = Just $ configFileWithTargets [] [] True
        cliOpts <- parseArgString cliParser "--only-target npm"
        filters <- filterSet <$> mergeOpts cfgFile envVars cliOpts
        case (combinedTargets $ includeFilters filters, combinedTargets $ excludeFilters filters) of
          (includedTargets, []) -> includedTargets `shouldBe'` [TypeTarget "npm"]
          _ -> expectationFailure' ("Incorrect filters applied. Got " ++ show filters)

      it' "should ignore config file specifying targets.only and just use CLI options" $ do
        let cfgFile = Just $ configFileWithTargets ["npm"] [] False
        cliOpts <- parseArgString cliParser "--only-target gomod"
        filters <- filterSet <$> mergeOpts cfgFile envVars cliOpts
        case (combinedTargets $ includeFilters filters, combinedTargets $ excludeFilters filters) of
          (includedTargets, []) -> includedTargets `shouldBe'` [TypeTarget "gomod"]
          _ -> expectationFailure' ("Incorrect filters applied. Got " ++ show filters)
