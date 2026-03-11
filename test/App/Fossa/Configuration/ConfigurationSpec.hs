{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Configuration.ConfigurationSpec (
  spec,
  expectedReleaseGroup,
) where

import App.Fossa.Config.ConfigFile (
  ConfigFile (..),
  ConfigGrepEntry (..),
  ConfigProject (..),
  ConfigReleaseGroup (..),
  ConfigReleaseGroupProject (..),
  ConfigRevision (..),
  ConfigTargets (..),
  ExperimentalConfigs (..),
  ExperimentalGradleConfigs (ExperimentalGradleConfigs),
  MavenScopeConfig (..),
  VendoredDependencyConfigs (..),
  resolveConfigFile,
 )
import App.Fossa.Lernie.Types (OrgWideCustomLicenseConfigPolicy (..))
import App.Types (Policy (PolicyName), ReleaseGroupMetadata (..))
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Stack (runStack)
import Data.Set qualified as Set
import Diag.Result (Result)
import Effect.Logger (ignoreLogger)
import Effect.ReadFS (runReadFSIO)
import Path (Abs, Dir, File, Path, Rel, mkRelDir, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import ResultUtil (assertOnSuccess, expectFailure)
import Test.Hspec qualified as T
import Test.Hspec.Core.Spec (SpecM)
import Text.RawString.QQ (r)
import Types (ArchiveUploadType (..), BuildTarget (..), GlobFilter (GlobFilter), LicenseScanPathFilters (..), TargetFilter (..))

expectedConfigFile :: Path Abs File -> ConfigFile
expectedConfigFile path =
  ConfigFile
    { configVersion = 3
    , configServer = Just "https://app.fossa.com"
    , configApiKey = Just "123"
    , configProject = Just expectedConfigProject
    , configReleaseGroup = Just expectedReleaseGroup
    , configRevision = Just expectedConfigRevision
    , configTargets = Just expectedConfigTargets
    , configPaths = Nothing
    , configExperimental = Just expectedExperimentalConfig
    , configMavenScope = Just expectedMavenScopeConfig
    , configVendoredDependencies = Just expectedVendoredDependencies
    , configTelemetry = Nothing
    , configCustomLicenseSearch = Just expectedLicenseSearch
    , configKeywordSearch = Just expectedKeywordSearch
    , configOrgWideCustomLicenseConfigPolicy = Use
    , configConfigFilePath = path
    , configReachability = Nothing
    }

expectedReleaseGroup :: ConfigReleaseGroup
expectedReleaseGroup =
  ConfigReleaseGroup
    { configReleaseGroupTitle = Just "example-title"
    , configReleaseGroupRelease = Just "example-release-title"
    , configReleaseGroupProjects = Just [expectedReleaseGroupProject]
    , configReleaseGroupLicensePolicy = Just "example-license-policy"
    , configReleaseGroupSecurityPolicy = Just "example-security-policy"
    , configReleaseGroupQualityPolicy = Just "example-quality-policy"
    , configReleaseGroupTeams = Just ["team-1", "team-2"]
    }

expectedReleaseGroupProject :: ConfigReleaseGroupProject
expectedReleaseGroupProject =
  ConfigReleaseGroupProject
    { configReleaseGroupProjectId = "custom+1/git@github.com/fossa-cli"
    , configReleaseGroupProjectRevision = "12345"
    , configReleaseGroupProjectBranch = "main"
    }

expectedConfigProject :: ConfigProject
expectedConfigProject =
  ConfigProject
    { configProjLocator = Just "custom+1/github.com/fossa-cli"
    , configProjID = Just "github.com/fossa-cli"
    , configName = Just "fossa-cli"
    , configLink = Just "fossa.com"
    , configTeam = Just "fossa-team"
    , configTeams = Just ["fossa-team"]
    , configJiraKey = Just "key"
    , configUrl = Just "fossa.com"
    , configPolicy = Just (PolicyName "license-policy")
    , configLabel = ["project-label", "label-2"]
    , configProjectReleaseGroup = Just expectedReleaseGroupMetadata
    , configPolicyId = Nothing
    }

expectedConfigRevision :: ConfigRevision
expectedConfigRevision =
  ConfigRevision
    { configCommit = Just "12345"
    , configBranch = Just "master"
    }

expectedReleaseGroupMetadata :: ReleaseGroupMetadata
expectedReleaseGroupMetadata =
  ReleaseGroupMetadata
    { releaseGroupName = "test-release"
    , releaseGroupRelease = "123"
    }

expectedConfigTargets :: ConfigTargets
expectedConfigTargets =
  ConfigTargets
    { targetsOnly = [directoryTarget, simpleTarget, complexTarget]
    , targetsExclude = []
    , targetsExcludeManifestStrategies = False
    }

expectedExperimentalConfig :: ExperimentalConfigs
expectedExperimentalConfig =
  ExperimentalConfigs
    { gradle = Just $ ExperimentalGradleConfigs (Set.fromList ["onlyProdConfigs", "onlyProdConfigs2"])
    }

expectedMavenScopeConfig :: MavenScopeConfig
expectedMavenScopeConfig =
  MavenScopeOnlyConfig (Set.fromList ["compile", "runtime"])

expectedVendoredDependencies :: VendoredDependencyConfigs
expectedVendoredDependencies =
  VendoredDependencyConfigs
    { configForceRescans = True
    , configLicenseScanMethod = Just ArchiveUpload
    , configLicenseScanPathFilters = Just expectedVendoredDependencyFilters
    }

expectedVendoredDependencyFilters :: LicenseScanPathFilters
expectedVendoredDependencyFilters =
  LicenseScanPathFilters
    { licenseScanPathFiltersOnly = [GlobFilter "**/*.rb"]
    , licenseScanPathFiltersExclude = [GlobFilter ".git/**", GlobFilter "test/**/*.rb"]
    , licenseScanPathFilterFileExclude = []
    }

expectedLicenseGrepEntry :: ConfigGrepEntry
expectedLicenseGrepEntry =
  ConfigGrepEntry
    { configGrepMatchCriteria = "(?i)proprietary"
    , configGrepName = "Proprietary License"
    }

expectedLicenseSearch :: [ConfigGrepEntry]
expectedLicenseSearch = [expectedLicenseGrepEntry]

expectedKeywordGrepEntry :: ConfigGrepEntry
expectedKeywordGrepEntry =
  ConfigGrepEntry
    { configGrepMatchCriteria = [r|\w*(_token)|]
    , configGrepName = "token search"
    }

expectedKeywordSearch :: [ConfigGrepEntry]
expectedKeywordSearch = [expectedKeywordGrepEntry]

simpleTarget :: TargetFilter
simpleTarget = TypeTarget "pip"

complexTarget :: TargetFilter
complexTarget = TypeDirTargetTarget "gradle" $(mkRelDir "./") (BuildTarget "specific-target")

directoryTarget :: TargetFilter
directoryTarget = TypeDirTarget "maven" $(mkRelDir "root")

maintestdir :: Path Rel Dir
maintestdir = $(mkRelDir "test/App/Fossa/Configuration/testdata")

ver2DefaultDir :: Path Rel Dir
ver2DefaultDir = maintestdir </> $(mkRelDir "ver2-default")

validDefaultDir :: Path Rel Dir
validDefaultDir = maintestdir </> $(mkRelDir "valid-default")

validDefaultYamlDir :: Path Rel Dir
validDefaultYamlDir = maintestdir </> $(mkRelDir "valid-default-yaml")

invalidDefaultDir :: Path Rel Dir
invalidDefaultDir = maintestdir </> $(mkRelDir "invalid-default")

invalidScanMethodDir :: Path Rel Dir
invalidScanMethodDir = maintestdir </> $(mkRelDir "invalid-scan-method")

invalidPoliciesDir :: Path Rel Dir
invalidPoliciesDir = maintestdir </> $(mkRelDir "invalid-policies")

expectSuccessfulParse :: Result (Maybe ConfigFile) -> Path Abs File -> T.Expectation
expectSuccessfulParse act configFilePath =
  assertOnSuccess act $ \_ a -> case a of
    Nothing -> T.expectationFailure "config file was Nothing after parsing"
    Just result -> result `T.shouldBe` (expectedConfigFile configFilePath)

spec :: T.Spec
spec = do
  curdir <- T.runIO getCurrentDir

  let runIt :: Path Rel Dir -> Maybe FilePath -> SpecM a (Result (Maybe ConfigFile))
      runIt dir maybeFile =
        T.runIO . runStack . ignoreLogger $
          Diag.runDiagnostics $
            runReadFSIO $
              resolveConfigFile (curdir </> dir) maybeFile

  -- @Nothing@ informs us to search for the default config file in that dir,
  -- so we use ignore/warn semantics, with one exception.
  validDefault <- runIt validDefaultDir Nothing
  validDefaultYaml <- runIt validDefaultYamlDir Nothing
  missingDefault <- runIt maintestdir Nothing
  ver2Default <- runIt ver2DefaultDir Nothing
  -- If the file exists, but is invalid YAML, that's most likely a real error,
  -- so we error here
  invalidDefault <- runIt invalidDefaultDir Nothing
  invalidScanMethod <- runIt invalidScanMethodDir Nothing
  invalidPolicies <- runIt invalidPoliciesDir Nothing

  -- @Just file@ informs us that the file is specified manually, so we fail
  -- instead of trying to recover, so we don't ignore the file and do the wrong thing
  validSpecified <- runIt validDefaultDir $ Just ".fossa.yml"
  invalidSpecified <- runIt maintestdir $ Just "invalid-specified.yml"
  missingSpecified <- runIt maintestdir $ Just "missing"
  ver2Specified <- runIt maintestdir $ Just "ver2-specified.yml"

  T.describe "config file parser" $ do
    T.it "parses a full config file in the default location" $ expectSuccessfulParse validDefault $ curdir </> validDefaultDir </> $(mkRelFile ".fossa.yml")

    T.it "parses a full config file with the `yaml` extension in the default location" $ expectSuccessfulParse validDefaultYaml $ curdir </> validDefaultYamlDir </> $(mkRelFile ".fossa.yaml")

    T.it "parses a full config file in a specified location" $ expectSuccessfulParse validSpecified $ curdir </> validDefaultDir </> $(mkRelFile ".fossa.yml")

    T.it "always returns failure for a bad file" $ do
      expectFailure invalidSpecified
      expectFailure invalidDefault
      expectFailure invalidScanMethod
      expectFailure invalidPolicies

    T.it "returns Nothing for missing default file" $
      assertOnSuccess missingDefault $
        \_ result -> result `T.shouldBe` Nothing

    T.it "returns Nothing for incompatible file" $
      assertOnSuccess ver2Default $
        \_ result -> result `T.shouldBe` Nothing

    T.it "fails for a missing specified file" $
      expectFailure missingSpecified

    T.it "fails for incompatible specified file" $
      expectFailure ver2Specified
