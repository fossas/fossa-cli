{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Config.ReleaseGroup.CreateSpec (spec, expectedReleaseGroupRevisionFromConfig, expectedReleaseGroupReleaseRevisionFromConfig) where

import App.Fossa.Config.ConfigFile (ConfigFile (..))
import App.Fossa.Config.EnvironmentVars (EnvVars (..))
import App.Fossa.Config.ReleaseGroup.Common (ReleaseGroupCommonOpts (..), ReleaseGroupProjectOpts (..))
import App.Fossa.Config.ReleaseGroup.Create (CreateConfig (..), CreateOpts (..), mergeOpts)
import App.Fossa.Config.Utils (fixtureDir)
import App.Fossa.Configuration.ConfigurationSpec (expectedReleaseGroup)
import App.Fossa.Ficus.Types (OrgWideCustomLicenseConfigPolicy (..))
import App.Types (ReleaseGroupProjectRevision (..), ReleaseGroupReleaseRevision (..), ReleaseGroupRevision (..))
import Path (Abs, File, Path, mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Test.Effect (expectFatal', it', shouldBe')
import Test.Hspec (Spec, describe)
import Test.Hspec.Core.Spec (runIO)

createOpts :: CreateOpts
createOpts =
  CreateOpts
    { releaseGroupCommon = commonOpts
    , configOpts = Nothing
    , titleOpts = Just "title-opts"
    , releaseOpts = Just "release-opts"
    , projectsOpts = Just [projectOpts]
    , licensePolicyOpts = Just "license-policy-opts"
    , securityPolicyOpts = Just "security-policy-opts"
    , qualityPolicyOpts = Just "quality-policy-opts"
    , teamsOpts = Just ["team1-opts", "team2-opts"]
    }

emptyReleaseCreateOpts :: CreateOpts
emptyReleaseCreateOpts =
  CreateOpts
    { releaseGroupCommon = commonOpts
    , configOpts = Nothing
    , titleOpts = Just "title-opts"
    , releaseOpts = Nothing
    , projectsOpts = Nothing
    , licensePolicyOpts = Nothing
    , securityPolicyOpts = Nothing
    , qualityPolicyOpts = Nothing
    , teamsOpts = Nothing
    }

emptyProjectsCreateOpts :: CreateOpts
emptyProjectsCreateOpts =
  CreateOpts
    { releaseGroupCommon = commonOpts
    , configOpts = Nothing
    , titleOpts = Just "title-opts"
    , releaseOpts = Just "release-opts"
    , projectsOpts = Nothing
    , licensePolicyOpts = Nothing
    , securityPolicyOpts = Nothing
    , qualityPolicyOpts = Nothing
    , teamsOpts = Nothing
    }

emptyCreateOpts :: CreateOpts
emptyCreateOpts =
  CreateOpts
    { releaseGroupCommon = commonOpts
    , configOpts = Nothing
    , titleOpts = Nothing
    , releaseOpts = Nothing
    , projectsOpts = Nothing
    , licensePolicyOpts = Nothing
    , securityPolicyOpts = Nothing
    , qualityPolicyOpts = Nothing
    , teamsOpts = Nothing
    }

configFile :: Path Abs File -> ConfigFile
configFile path =
  ConfigFile
    { configVersion = 3
    , configServer = Nothing
    , configApiKey = Nothing
    , configReleaseGroup = Just expectedReleaseGroup
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
    , configReachability = Nothing
    , configOrgWideCustomLicenseConfigPolicy = Use
    , configConfigFilePath = path
    }

projectOpts :: ReleaseGroupProjectOpts
projectOpts =
  ReleaseGroupProjectOpts
    { projectLocatorOpts = "project-locator-opts"
    , projectRevisionOpts = "project-revision-opts"
    , projectBranchOpts = "project-branch-opts"
    }

commonOpts :: ReleaseGroupCommonOpts
commonOpts =
  ReleaseGroupCommonOpts
    { debug = False
    , baseUrl = Nothing
    , apiKey = Just "123"
    }

expectedReleaseGroupRevisionFromOpts :: ReleaseGroupRevision
expectedReleaseGroupRevisionFromOpts =
  ReleaseGroupRevision
    { releaseGroupTitle = "title-opts"
    , releaseGroupReleaseRevision = expectedReleaseGroupReleaseRevisionFromOpts
    , releaseGroupLicensePolicy = Just "license-policy-opts"
    , releaseGroupSecurityPolicy = Just "security-policy-opts"
    , releaseGroupQualityPolicy = Just "quality-policy-opts"
    , releaseGroupTeams = Just ["team1-opts", "team2-opts"]
    }

expectedReleaseGroupReleaseRevisionFromOpts :: ReleaseGroupReleaseRevision
expectedReleaseGroupReleaseRevisionFromOpts =
  ReleaseGroupReleaseRevision
    { releaseTitle = "release-opts"
    , releaseProjects = [expectedReleaseGroupProjectRevisionFromOpts]
    }

expectedReleaseGroupProjectRevisionFromOpts :: ReleaseGroupProjectRevision
expectedReleaseGroupProjectRevisionFromOpts =
  ReleaseGroupProjectRevision
    { releaseGroupProjectLocator = "project-locator-opts"
    , releaseGroupProjectRevision = "project-locator-opts$project-revision-opts"
    , releaseGroupProjectBranch = "project-branch-opts"
    }

expectedReleaseGroupRevisionFromConfig :: ReleaseGroupRevision
expectedReleaseGroupRevisionFromConfig =
  ReleaseGroupRevision
    { releaseGroupTitle = "example-title"
    , releaseGroupReleaseRevision = expectedReleaseGroupReleaseRevisionFromConfig
    , releaseGroupLicensePolicy = Just "example-license-policy"
    , releaseGroupSecurityPolicy = Just "example-security-policy"
    , releaseGroupQualityPolicy = Just "example-quality-policy"
    , releaseGroupTeams = Just ["team-1", "team-2"]
    }

expectedReleaseGroupReleaseRevisionFromConfig :: ReleaseGroupReleaseRevision
expectedReleaseGroupReleaseRevisionFromConfig =
  ReleaseGroupReleaseRevision
    { releaseTitle = "example-release-title"
    , releaseProjects = [expectedReleaseGroupProjectRevisionFromConfig]
    }

expectedReleaseGroupProjectRevisionFromConfig :: ReleaseGroupProjectRevision
expectedReleaseGroupProjectRevisionFromConfig =
  ReleaseGroupProjectRevision
    { releaseGroupProjectLocator = "custom+1/git@github.com/fossa-cli"
    , releaseGroupProjectRevision = "custom+1/git@github.com/fossa-cli$12345"
    , releaseGroupProjectBranch = "main"
    }

spec :: Spec
spec = do
  currDir <- runIO getCurrentDir
  let scanDir = currDir </> fixtureDir
      absFilePath = scanDir </> $(mkRelFile ".fossa.yml")
      envVars = EnvVars Nothing False False Nothing Nothing mempty
  describe "mergeOpts" $ do
    it' "should use values from create opts when both create opts and config values are present" $ do
      createConfig <- mergeOpts (Just $ configFile absFilePath) envVars createOpts
      shouldBe' expectedReleaseGroupRevisionFromOpts $ releaseGroupRevision createConfig
    it' "should use values from config when create opts values are empty" $ do
      createConfig <- mergeOpts (Just $ configFile absFilePath) envVars emptyCreateOpts
      shouldBe' expectedReleaseGroupRevisionFromConfig $ releaseGroupRevision createConfig
    it' "should fail when no title is provided" $ do
      expectFatal' $ mergeOpts Nothing envVars emptyCreateOpts
    it' "should fail when no release is provided" $ do
      expectFatal' $ mergeOpts Nothing envVars emptyReleaseCreateOpts
    it' "should fail when no projects are provided" $ do
      expectFatal' $ mergeOpts Nothing envVars emptyProjectsCreateOpts
