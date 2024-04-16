{-# LANGUAGE GADTs #-}

module App.Fossa.API.BuildLinkSpec (spec) where

import App.Fossa.API.BuildLink (getBuildURLWithOrg, getFossaBuildUrl)
import App.Types (ProjectRevision (ProjectRevision))
import Control.Effect.FossaApiClient (
  FossaApiClientF (GetApiOpts, GetOrganization),
 )
import Data.Text (Text)
import Fossa.API.Types (
  OrgId (OrgId),
  Organization (Organization),
 )
import Fossa.API.Types qualified as API
import Srclib.Types (Locator (Locator))
import Test.Effect (it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)
import Test.MockApi (returnsOnce)
import Types (ArchiveUploadType (..))

simpleSamlPath :: Text
simpleSamlPath = "https://app.fossa.com/account/saml/1?next=/projects/fetcher123%252bproject123/refs/branch/master123/revision123"

-- All reserved characters in the 'next' URI get encoded once during render of
-- the query value. Then the resulting '%' symbols are themselves
-- percent-encoded during final rendering of the URI.  For example, the process
-- for '@' works this way: '@' -> '%40' -> '%2540'
gitSamlPath :: Text
gitSamlPath = "https://app.fossa.com/account/saml/103?next=/projects/fetcher%2540123%252fabc%252bgit%2540github.com%252fuser%252frepo/refs/branch/weird--branch/revision%2540123%252fabc"

fullSamlURL :: Text
fullSamlURL = "https://app.fossa.com/account/saml/33?next=/projects/a%252bb/refs/branch/master/c"

simpleStandardURL :: Text
simpleStandardURL = "https://app.fossa.com/projects/haskell%2b89%2fspectrometer/refs/branch/master/revision123"

spec :: Spec
spec = do
  describe "BuildLink" $ do
    describe "SAML URL builder" $ do
      it' "should render simple locators" $ do
        let locator = Locator "fetcher123" "project123" $ Just "revision123"
            org = Just $ Organization (OrgId 1) True False True CLILicenseScan True True True False False False False [] False False API.Free
            revision = ProjectRevision "" "not this revision" $ Just "master123"
        actual <- getBuildURLWithOrg org revision Fixtures.apiOpts locator

        actual `shouldBe'` simpleSamlPath

      it' "should render git@ locators" $ do
        let locator = Locator "fetcher@123/abc" "git@github.com/user/repo" $ Just "revision@123/abc"
            org = Just $ Organization (OrgId 103) True False True CLILicenseScan True True True False False False False [] False False API.Free
            revision = ProjectRevision "not this project name" "not this revision" $ Just "weird--branch"
        actual <- getBuildURLWithOrg org revision Fixtures.apiOpts locator

        actual `shouldBe'` gitSamlPath

      it' "should render full url correctly" $ do
        let locator = Locator "a" "b" $ Just "c"
            org = Just $ Organization (OrgId 33) True False True CLILicenseScan True True True False False False False [] False False API.Free
            revision = ProjectRevision "" "not this revision" $ Just "master"
        actual <- getBuildURLWithOrg org revision Fixtures.apiOpts locator

        actual `shouldBe'` fullSamlURL

    describe "Standard URL Builder" $ do
      it' "should render simple links" $ do
        let locator = Locator "haskell" "89/spectrometer" $ Just "revision123"
            revision = ProjectRevision "" "not this revision" $ Just "master"
        actual <- getBuildURLWithOrg Nothing revision Fixtures.apiOpts locator

        actual `shouldBe'` simpleStandardURL

    describe "Fossa URL Builder" $
      it' "should render from API info" $ do
        GetApiOpts `returnsOnce` Fixtures.apiOpts
        GetOrganization `returnsOnce` Organization (OrgId 1) True False True CLILicenseScan True True True False False False False [] False False API.Free
        let locator = Locator "fetcher123" "project123" $ Just "revision123"
            revision = ProjectRevision "" "not this revision" $ Just "master123"
        actual <- getFossaBuildUrl revision locator

        actual `shouldBe'` simpleSamlPath
