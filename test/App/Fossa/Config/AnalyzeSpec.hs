{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Config.AnalyzeSpec (spec) where

import App.Fossa.Config.Analyze (
  AnalyzeConfig (filterSet, xWorkflow),
  cliParser,
  loadConfig,
  mergeOpts,
 )
import App.Fossa.Config.ConfigFile (ConfigFile (..), ConfigTargets (..))
import App.Fossa.Config.EnvironmentVars (EnvVars (..))
import App.Fossa.Config.Utils (itShouldFailWhenLabelsExceedFive, itShouldLoadFromTheConfiguredBaseDir, parseArgString)
import App.Fossa.Lernie.Types (OrgWideCustomLicenseConfigPolicy (..))
import Control.Effect.Diagnostics (Diagnostics, errorBoundary)
import Control.Effect.Lift (Has, Lift, sendIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Diag.Result (Result (Failure, Success), renderFailure)
import Discovery.Filters (AllFilters (..), combinedTargets)
import Effect.Logger (renderIt)
import Path (Abs, Dir, File, Path, mkAbsFile, mkRelFile, toFilePath, (</>))
import Test.Effect (expectFatal', expectationFailure', it', itWithTempDir', shouldBe', shouldEndWith')
import Test.Hspec (Spec, describe)
import Types (DiscoveredProjectType, TargetFilter (TypeTarget))

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

configPath :: Path Abs File
#ifdef mingw32_HOST_OS
configPath = $(mkAbsFile "C:/.fossa.yml")
#else
configPath = $(mkAbsFile "/tmp/.fossa.yml")
#endif

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
    , configConfigFilePath = configPath
    }

numberOfStrategies :: Int
numberOfStrategies = length allProjectTypes
  where
    allProjectTypes :: [DiscoveredProjectType]
    allProjectTypes = enumFromTo minBound maxBound

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
        filters <- filterSet <$> mergeOpts Nothing cfgFile envVars cliOpts
        case (combinedTargets $ includeFilters filters, combinedTargets $ excludeFilters filters) of
          ([], excludedTargets) -> length excludedTargets `shouldBe'` numberOfStrategies
          _ -> expectationFailure' ("Incorrect filters applied. Got " ++ show filters)

      it' "should set correct filters when only filter is set" $ do
        let cfgFile = Nothing
        cliOpts <- parseArgString cliParser "--only-target npm"
        filters <- filterSet <$> mergeOpts Nothing cfgFile envVars cliOpts
        case (combinedTargets $ includeFilters filters, combinedTargets $ excludeFilters filters) of
          (includedTargets, []) -> includedTargets `shouldBe'` [TypeTarget "npm"]
          _ -> expectationFailure' ("Incorrect filters applied. Got " ++ show filters)

      it' "should have --exclude-manifest-strategies override only/exclude filters" $ do
        let cfgFile = Nothing
        cliOpts <- parseArgString cliParser "--exclude-manifest-strategies --only-target npm"
        filters <- filterSet <$> mergeOpts Nothing cfgFile envVars cliOpts
        case (combinedTargets $ includeFilters filters, combinedTargets $ excludeFilters filters) of
          ([], excludedTargets) -> length excludedTargets `shouldBe'` numberOfStrategies
          _ -> expectationFailure' ("Incorrect filters applied. Got " ++ show filters)

    describe "only config file" $ do
      it' "should set correct filters when targets.excludeManifestStrategies is set" $ do
        let cfgFile = Just $ configFileWithTargets [] [] True
        cliOpts <- parseArgString cliParser ""
        filters <- filterSet <$> mergeOpts Nothing cfgFile envVars cliOpts
        case (combinedTargets $ includeFilters filters, combinedTargets $ excludeFilters filters) of
          ([], excludedTargets) -> length excludedTargets `shouldBe'` numberOfStrategies
          _ -> expectationFailure' ("Incorrect filters applied. Got " ++ show filters)

      it' "should set correct filters when targets.only is set" $ do
        let cfgFile = Just $ configFileWithTargets ["npm"] [] False
        cliOpts <- parseArgString cliParser ""
        filters <- filterSet <$> mergeOpts Nothing cfgFile envVars cliOpts
        case (combinedTargets $ includeFilters filters, combinedTargets $ excludeFilters filters) of
          (includedTargets, []) -> includedTargets `shouldBe'` [TypeTarget "npm"]
          _ -> expectationFailure' ("Incorrect filters applied. Got " ++ show filters)

      it' "should have targets.excludeManifestStrategies override only/exclude filters" $ do
        let cfgFile = Just $ configFileWithTargets ["npm"] [] True
        cliOpts <- parseArgString cliParser ""
        filters <- filterSet <$> mergeOpts Nothing cfgFile envVars cliOpts
        case (combinedTargets $ includeFilters filters, combinedTargets $ excludeFilters filters) of
          ([], excludedTargets) -> length excludedTargets `shouldBe'` numberOfStrategies
          _ -> expectationFailure' ("Incorrect filters applied. Got " ++ show filters)

    describe "config file and CLI options" $ do
      it' "should ignore config file specifying targets.excludeManifestStrategies and just use CLI options" $ do
        let cfgFile = Just $ configFileWithTargets [] [] True
        cliOpts <- parseArgString cliParser "--only-target npm"
        filters <- filterSet <$> mergeOpts Nothing cfgFile envVars cliOpts
        case (combinedTargets $ includeFilters filters, combinedTargets $ excludeFilters filters) of
          (includedTargets, []) -> includedTargets `shouldBe'` [TypeTarget "npm"]
          _ -> expectationFailure' ("Incorrect filters applied. Got " ++ show filters)

      it' "should ignore config file specifying targets.only and just use CLI options" $ do
        let cfgFile = Just $ configFileWithTargets ["npm"] [] False
        cliOpts <- parseArgString cliParser "--only-target gomod"
        filters <- filterSet <$> mergeOpts Nothing cfgFile envVars cliOpts
        case (combinedTargets $ includeFilters filters, combinedTargets $ excludeFilters filters) of
          (includedTargets, []) -> includedTargets `shouldBe'` [TypeTarget "gomod"]
          _ -> expectationFailure' ("Incorrect filters applied. Got " ++ show filters)

  describe "incompatible flags" $ do
    it' "should fail when --snippet-scan and --output are used together" $ do
      cliOpts <- parseArgString cliParser "--snippet-scan --output"
      expectFatal' $ mergeOpts Nothing Nothing envVars cliOpts

    it' "should fail when --x-vendetta and --output are used together" $ do
      cliOpts <- parseArgString cliParser "--x-vendetta --output"
      expectFatal' $ mergeOpts Nothing Nothing envVars cliOpts

  describe "--x-workflow" $ do
    it' "should default to Nothing when the flag is absent" $ do
      cliOpts <- parseArgString cliParser ""
      workflow <- xWorkflow <$> mergeOpts Nothing Nothing envVars cliOpts
      workflow `shouldBe'` Nothing

    itWithTempDir' "should resolve the flag to an absolute path" $ \tmpDir -> do
      analyzer <- writeAnalyzer tmpDir
      cliOpts <- parseArgString cliParser $ "--x-workflow " <> toFilePath analyzer
      workflow <- xWorkflow <$> mergeOpts Nothing Nothing envVars cliOpts
      case workflow of
        Nothing -> expectationFailure' "expected --x-workflow to resolve to a path"
        Just resolved -> toFilePath resolved `shouldEndWith'` "analyzer.js"

    it' "should fail when the named analyzer does not exist" $ do
      cliOpts <- parseArgString cliParser "--x-workflow /definitely/not/here/analyzer.js"
      expectFatal' $ mergeOpts Nothing Nothing envVars cliOpts

    itWithTempDir' "should fail when combined with --static-only-analysis" $ \tmpDir -> do
      analyzer <- writeAnalyzer tmpDir
      cliOpts <- parseArgString cliParser $ "--static-only-analysis --x-workflow " <> toFilePath analyzer
      failureText <- renderedFailure $ mergeOpts Nothing Nothing envVars cliOpts
      case failureText of
        Nothing -> expectationFailure' "expected --static-only-analysis with --x-workflow to be fatal"
        Just rendered -> Text.isInfixOf "--static-only-analysis" rendered `shouldBe'` True

-- | Create a file the CLI can resolve, so path resolution is never the reason a test fails.
writeAnalyzer :: (Has (Lift IO) sig m) => Path Abs Dir -> m (Path Abs File)
writeAnalyzer tmpDir = do
  let analyzer = tmpDir </> $(mkRelFile "analyzer.js")
  sendIO $ writeFile (toFilePath analyzer) "// stub\n"
  pure analyzer

-- | 'expectFatal'' only reports that a failure happened; the message is what
-- distinguishes the flag conflict from an unrelated failure on the same path.
renderedFailure :: (Has (Lift IO) sig m, Has Diagnostics sig m) => m a -> m (Maybe Text)
renderedFailure act =
  errorBoundary act >>= \case
    Failure ws eg -> pure . Just . renderIt $ renderFailure ws eg "An issue occurred"
    Success _ _ -> pure Nothing
