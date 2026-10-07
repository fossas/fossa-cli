{-# LANGUAGE CPP #-}

module App.Fossa.Config.AnalyzeSpec (spec) where

import App.Fossa.Config.Analyze (
  AnalyzeConfig (filterSet, snippetScanSkipHeaders, xWorkflow),
  SkipHeadersOptions (..),
  cliParser,
  loadConfig,
  mergeOpts,
 )
import App.Fossa.Config.ConfigFile (ConfigFile (..), ConfigTargets (..), SnippetScanConfigs (..), VendoredDependencyConfigs (..))
import App.Fossa.Config.EnvironmentVars (EnvVars (..))
import App.Fossa.Config.Utils (itShouldFailWhenLabelsExceedFive, itShouldLoadFromTheConfiguredBaseDir, parseArgString)
import App.Fossa.Lernie.Types (OrgWideCustomLicenseConfigPolicy (..))
import Control.Carrier.Writer.Strict (runWriter)
import Control.Effect.Diagnostics (Diagnostics, errorBoundary)
import Control.Effect.Lift (Has, Lift)
import Control.Exception (throw)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Diag.Result (Result (Failure, Success), renderFailure)
import Discovery.Filters (AllFilters (..), combinedTargets)
import Effect.Logger (Severity (SevWarn), renderIt, withWriterLogger)
import Options.Applicative (defaultPrefs, execParserPure, getParseResult, info)
import Path (Abs, File, Path, parseAbsFile)
import Test.Effect (expectFatal', expectationFailure', it', shouldBe')
import Test.Hspec (Spec, describe)
import Types (DiscoveredProjectType, TargetFilter (TypeTarget))

-- | The fixtures below are valid paths on the platform the tests run on; a
-- parse failure means the fixture itself is broken, so fail with the parse
-- error rather than carrying it on to an assertion.
mustParse :: (Show e) => (String -> Either e p) -> String -> p
mustParse f s = either (throw . userError . show) id (f s)

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
configPath = mustParse parseAbsFile "C:/.fossa.yml"
#else
configPath = mustParse parseAbsFile "/tmp/.fossa.yml"
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

-- | A config file that only sets @vendoredDependencies.snippetScan@.
configFileWithSnippetScan :: Bool -> Maybe Int -> ConfigFile
configFileWithSnippetScan skipHeaders limit =
  (configFileWithTargets [] [] False)
    { configTargets = Nothing
    , configVendoredDependencies =
        Just $
          VendoredDependencyConfigs
            { configForceRescans = False
            , configLicenseScanMethod = Nothing
            , configLicenseScanPathFilters = Nothing
            , configSnippetScan = Just $ SnippetScanConfigs skipHeaders limit
            }
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

  describe "--snippet-scan-skip-headers" $ do
    it' "should be off by default" $ do
      cliOpts <- parseArgString cliParser "--snippet-scan"
      skipHeaders <- snippetScanSkipHeaders <$> mergeOpts Nothing Nothing envVars cliOpts
      skipHeaders `shouldBe'` Nothing

    it' "should enable header skipping without a limit" $ do
      cliOpts <- parseArgString cliParser "--snippet-scan --snippet-scan-skip-headers"
      skipHeaders <- snippetScanSkipHeaders <$> mergeOpts Nothing Nothing envVars cliOpts
      skipHeaders `shouldBe'` Just (SkipHeadersOptions Nothing)

    it' "should enable header skipping with a limit" $ do
      cliOpts <- parseArgString cliParser "--snippet-scan --snippet-scan-skip-headers --snippet-scan-skip-headers-limit 20"
      skipHeaders <- snippetScanSkipHeaders <$> mergeOpts Nothing Nothing envVars cliOpts
      skipHeaders `shouldBe'` Just (SkipHeadersOptions (Just 20))

    it' "should accept a limit of 0" $ do
      cliOpts <- parseArgString cliParser "--snippet-scan --snippet-scan-skip-headers --snippet-scan-skip-headers-limit 0"
      skipHeaders <- snippetScanSkipHeaders <$> mergeOpts Nothing Nothing envVars cliOpts
      skipHeaders `shouldBe'` Just (SkipHeadersOptions (Just 0))

    it' "should work with the deprecated --x-snippet-scan" $ do
      cliOpts <- parseArgString cliParser "--x-snippet-scan --snippet-scan-skip-headers"
      skipHeaders <- snippetScanSkipHeaders <$> mergeOpts Nothing Nothing envVars cliOpts
      skipHeaders `shouldBe'` Just (SkipHeadersOptions Nothing)

    it' "should reject a negative limit at parse time" $ do
      let parsed = getParseResult $ execParserPure defaultPrefs (info cliParser mempty) ["--snippet-scan", "--snippet-scan-skip-headers", "--snippet-scan-skip-headers-limit=-1"]
      isNothing parsed `shouldBe'` True

    it' "should reject a non-numeric limit at parse time" $ do
      let parsed = getParseResult $ execParserPure defaultPrefs (info cliParser mempty) ["--snippet-scan", "--snippet-scan-skip-headers", "--snippet-scan-skip-headers-limit=many"]
      isNothing parsed `shouldBe'` True

    it' "should fail without --snippet-scan" $ do
      cliOpts <- parseArgString cliParser "--snippet-scan-skip-headers"
      failureText <- renderedFailure $ mergeOpts Nothing Nothing envVars cliOpts
      case failureText of
        Nothing -> expectationFailure' "expected --snippet-scan-skip-headers without --snippet-scan to be fatal"
        Just rendered -> Text.isInfixOf "require --snippet-scan" rendered `shouldBe'` True

    it' "should fail when the limit is used without --snippet-scan" $ do
      cliOpts <- parseArgString cliParser "--snippet-scan-skip-headers --snippet-scan-skip-headers-limit 5"
      failureText <- renderedFailure $ mergeOpts Nothing Nothing envVars cliOpts
      case failureText of
        Nothing -> expectationFailure' "expected --snippet-scan-skip-headers-limit without --snippet-scan to be fatal"
        Just rendered -> Text.isInfixOf "require --snippet-scan" rendered `shouldBe'` True

    it' "should fail when the limit is used without --snippet-scan-skip-headers" $ do
      cliOpts <- parseArgString cliParser "--snippet-scan --snippet-scan-skip-headers-limit 5"
      failureText <- renderedFailure $ mergeOpts Nothing Nothing envVars cliOpts
      case failureText of
        Nothing -> expectationFailure' "expected --snippet-scan-skip-headers-limit without --snippet-scan-skip-headers to be fatal"
        Just rendered -> Text.isInfixOf "has no effect without --snippet-scan-skip-headers" rendered `shouldBe'` True

  describe "vendoredDependencies.snippetScan in the config file" $ do
    let mergeWithWarnings cfgFile args = do
          cliOpts <- parseArgString cliParser args
          (warnings, cfg) <- runWriter . withWriterLogger @[] SevWarn $ mergeOpts Nothing (Just cfgFile) envVars cliOpts
          pure (map renderIt warnings, snippetScanSkipHeaders cfg)

    it' "should enable header skipping from the config file alone" $ do
      (warnings, skipHeaders) <- mergeWithWarnings (configFileWithSnippetScan True (Just 20)) "--snippet-scan"
      skipHeaders `shouldBe'` Just (SkipHeadersOptions (Just 20))
      warnings `shouldBe'` []

    it' "should enable header skipping from the config file without a limit" $ do
      (_, skipHeaders) <- mergeWithWarnings (configFileWithSnippetScan True Nothing) "--snippet-scan"
      skipHeaders `shouldBe'` Just (SkipHeadersOptions Nothing)

    it' "should use the config file limit with the CLI flag" $ do
      (_, skipHeaders) <- mergeWithWarnings (configFileWithSnippetScan False (Just 20)) "--snippet-scan --snippet-scan-skip-headers"
      skipHeaders `shouldBe'` Just (SkipHeadersOptions (Just 20))

    it' "should let the CLI limit override the config file limit" $ do
      (_, skipHeaders) <- mergeWithWarnings (configFileWithSnippetScan True (Just 20)) "--snippet-scan --snippet-scan-skip-headers --snippet-scan-skip-headers-limit 5"
      skipHeaders `shouldBe'` Just (SkipHeadersOptions (Just 5))

    it' "should accept a CLI limit when only the config file enables header skipping" $ do
      (_, skipHeaders) <- mergeWithWarnings (configFileWithSnippetScan True Nothing) "--snippet-scan --snippet-scan-skip-headers-limit 5"
      skipHeaders `shouldBe'` Just (SkipHeadersOptions (Just 5))

    it' "should leave header skipping off when the config file does not enable it" $ do
      (warnings, skipHeaders) <- mergeWithWarnings (configFileWithSnippetScan False Nothing) "--snippet-scan"
      skipHeaders `shouldBe'` Nothing
      warnings `shouldBe'` []

    it' "should silently ignore the config file without --snippet-scan" $ do
      (warnings, skipHeaders) <- mergeWithWarnings (configFileWithSnippetScan True (Just 20)) ""
      skipHeaders `shouldBe'` Nothing
      warnings `shouldBe'` []

    it' "should silently ignore a config file limit without skipHeaders when not snippet scanning" $ do
      (warnings, skipHeaders) <- mergeWithWarnings (configFileWithSnippetScan False (Just 20)) ""
      skipHeaders `shouldBe'` Nothing
      warnings `shouldBe'` []

    it' "should warn about a config file limit without skipHeaders and not emit it" $ do
      (warnings, skipHeaders) <- mergeWithWarnings (configFileWithSnippetScan False (Just 20)) "--snippet-scan"
      skipHeaders `shouldBe'` Nothing
      any (Text.isInfixOf "skipHeadersLimit") warnings `shouldBe'` True

    it' "should still fail on a CLI limit when neither source enables header skipping" $ do
      cliOpts <- parseArgString cliParser "--snippet-scan --snippet-scan-skip-headers-limit 5"
      failureText <- renderedFailure $ mergeOpts Nothing (Just $ configFileWithSnippetScan False Nothing) envVars cliOpts
      case failureText of
        Nothing -> expectationFailure' "expected --snippet-scan-skip-headers-limit without header skipping to be fatal"
        Just rendered -> Text.isInfixOf "has no effect without --snippet-scan-skip-headers" rendered `shouldBe'` True

    it' "should still fail on CLI flags without --snippet-scan when the config file enables header skipping" $ do
      cliOpts <- parseArgString cliParser "--snippet-scan-skip-headers"
      failureText <- renderedFailure $ mergeOpts Nothing (Just $ configFileWithSnippetScan True Nothing) envVars cliOpts
      case failureText of
        Nothing -> expectationFailure' "expected --snippet-scan-skip-headers without --snippet-scan to be fatal"
        Just rendered -> Text.isInfixOf "require --snippet-scan" rendered `shouldBe'` True

  describe "--x-workflow" $ do
    it' "should default to False when the flag is absent" $ do
      cliOpts <- parseArgString cliParser ""
      workflow <- xWorkflow <$> mergeOpts Nothing Nothing envVars cliOpts
      workflow `shouldBe'` False

    it' "should enable the workflow when the flag is present" $ do
      cliOpts <- parseArgString cliParser "--x-workflow"
      workflow <- xWorkflow <$> mergeOpts Nothing Nothing envVars cliOpts
      workflow `shouldBe'` True

    -- The flag takes no argument, so a stray one is a target directory rather
    -- than an analyzer path. Pinned because it used to name the analyzer, and
    -- an old invocation must not silently analyze that path instead.
    it' "should treat a following path as the scan target, not an analyzer" $ do
      cliOpts <- parseArgString cliParser "--x-workflow /definitely/not/here"
      failureText <- renderedFailure $ mergeOpts Nothing Nothing envVars cliOpts
      case failureText of
        Nothing -> expectationFailure' "expected the trailing path to be read as the scan target"
        -- Don't assert on the full POSIX path literal: Windows normalises
        -- separators/drive letters, so "/definitely/not/here" never appears
        -- verbatim in the rendered message there. Instead assert (a) the
        -- failure is a missing-*directory* error -- the platform-independent
        -- marker that the argument reached 'validateDir' (the scan-target
        -- path), not the flag itself -- and (b) it still names a distinctive,
        -- separator-free fragment of the path, so this can't pass on some
        -- unrelated directory-not-found error.
        Just rendered ->
          (Text.isInfixOf "Directory does not exist" rendered && Text.isInfixOf "definitely" rendered) `shouldBe'` True

    it' "should fail when combined with --static-only-analysis" $ do
      cliOpts <- parseArgString cliParser "--static-only-analysis --x-workflow"
      failureText <- renderedFailure $ mergeOpts Nothing Nothing envVars cliOpts
      case failureText of
        Nothing -> expectationFailure' "expected --static-only-analysis with --x-workflow to be fatal"
        Just rendered -> Text.isInfixOf "--static-only-analysis" rendered `shouldBe'` True

-- | 'expectFatal'' only reports that a failure happened; the message is what
-- distinguishes the flag conflict from an unrelated failure on the same path.
renderedFailure :: (Has (Lift IO) sig m, Has Diagnostics sig m) => m a -> m (Maybe Text)
renderedFailure act =
  errorBoundary act >>= \case
    Failure ws eg -> pure . Just . renderIt $ renderFailure ws eg "An issue occurred"
    Success _ _ -> pure Nothing
