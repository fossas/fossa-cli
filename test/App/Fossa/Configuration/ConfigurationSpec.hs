{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Configuration.ConfigurationSpec (
  spec,
) where

import App.Fossa.Config.ConfigFile (
  ConfigFile (..),
  ConfigProject (..),
  ConfigRevision (..),
  ConfigTargets (..),
  ExperimentalConfigs (..),
  ExperimentalGradleConfigs (ExperimentalGradleConfigs),
  VendoredDependencyConfigs (..),
  resolveConfigFile,
 )
import App.Types (ReleaseGroupMetadata (..))
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Stack (runStack)
import Data.Set qualified as Set
import Diag.Result (Result)
import Effect.Logger (ignoreLogger)
import Effect.ReadFS (runReadFSIO)
import Path (Dir, Path, Rel, mkRelDir, (</>))
import Path.IO (getCurrentDir)
import ResultUtil (assertOnSuccess, expectFailure)
import Test.Hspec qualified as T
import Test.Hspec.Core.Spec (SpecM)
import Types (ArchiveUploadType (..), BuildTarget (..), GlobFilter (GlobFilter), TargetFilter (..), LicenseScanPathFilters (..))

expectedConfigFile :: ConfigFile
expectedConfigFile =
  ConfigFile
    { configVersion = 3
    , configServer = Just "https://app.fossa.com"
    , configApiKey = Just "123"
    , configProject = Just expectedConfigProject
    , configRevision = Just expectedConfigRevision
    , configTargets = Just expectedConfigTargets
    , configPaths = Nothing
    , configExperimental = Just expectedExperimentalConfig
    , configVendoredDependencies = Just expectedVendoredDependencies
    , configTelemetry = Nothing
    }

expectedConfigProject :: ConfigProject
expectedConfigProject =
  ConfigProject
    { configProjID = Just "github.com/fossa-cli"
    , configName = Just "fossa-cli"
    , configLink = Just "fossa.com"
    , configTeam = Just "fossa-team"
    , configJiraKey = Just "key"
    , configUrl = Just "fossa.com"
    , configPolicy = Just "license-policy"
    , configReleaseGroup = Just expectedReleaseGroup
    }

expectedConfigRevision :: ConfigRevision
expectedConfigRevision =
  ConfigRevision
    { configCommit = Just "12345"
    , configBranch = Just "master"
    }

expectedReleaseGroup :: ReleaseGroupMetadata
expectedReleaseGroup =
  ReleaseGroupMetadata
    { releaseGroupName = "test-release"
    , releaseGroupRelease = "123"
    }

expectedConfigTargets :: ConfigTargets
expectedConfigTargets =
  ConfigTargets
    { targetsOnly = [directoryTarget, simpleTarget, complexTarget]
    , targetsExclude = []
    }

expectedExperimentalConfig :: ExperimentalConfigs
expectedExperimentalConfig =
  ExperimentalConfigs
    { gradle = Just $ ExperimentalGradleConfigs (Set.fromList ["onlyProdConfigs", "onlyProdConfigs2"])
    }

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
    }

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

expectSuccessfulParse :: Result (Maybe ConfigFile) -> T.Expectation
expectSuccessfulParse act =
  assertOnSuccess act $ \_ a -> case a of
    Nothing -> T.expectationFailure "config file was Nothing after parsing"
    Just result -> result `T.shouldBe` expectedConfigFile

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

  -- @Just file@ informs us that the file is specified manually, so we fail
  -- instead of trying to recover, so we don't ignore the file and do the wrong thing
  validSpecified <- runIt validDefaultDir $ Just ".fossa.yml"
  invalidSpecified <- runIt maintestdir $ Just "invalid-specified.yml"
  missingSpecified <- runIt maintestdir $ Just "missing"
  ver2Specified <- runIt maintestdir $ Just "ver2-specified.yml"

  T.describe "config file parser" $ do
    T.it "parses a full config file in the default location" $ expectSuccessfulParse validDefault

    T.it "parses a full config file with the `yaml` extension in the default location" $ expectSuccessfulParse validDefaultYaml

    T.it "parses a full config file in a specified location" $ expectSuccessfulParse validSpecified

    T.it "always returns failure for a bad file" $ do
      expectFailure invalidSpecified
      expectFailure invalidDefault
      expectFailure invalidScanMethod

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
