{-# LANGUAGE QuasiQuotes #-}

module App.Fossa.API.BuildLinkSpec (spec) where

import Test.Hspec
import App.Fossa.API.BuildLink
import Srclib.Types (Locator(Locator))
import App.Fossa.FossaAPIV1 (Organization(Organization))
import App.Types (ProjectRevision(ProjectRevision))
import Data.Text (Text)
import Fossa.API.Types
import Text.URI.QQ

simpleSamlPath :: Text
simpleSamlPath = "https://app.fossa.com/account/saml/1?next=%2Fprojects%2Ffetcher123%252Bproject123%2Frefs%2Fbranch%2Fmaster123%2Frevision123"

-- | Note the differences here between '%2F' and '%252F'.  The percent sign is re-encoded so that it's properly handled on the next redirect.
gitSamlPath :: Text
gitSamlPath = "https://app.fossa.com/account/saml/103?next=%2Fprojects%2Ffetcher%2540123%252Fabc%252Bgit%2540github.com%252Fuser%252Frepo%2Frefs%2Fbranch%2Fweird--branch%2Frevision%2540123%252Fabc"

fullSamlURL :: Text
fullSamlURL = "https://app.fossa.com/account/saml/33?next=%2Fprojects%2Fa%252Bb%2Frefs%2Fbranch%2Fmaster%2Fc"

simpleStandardURL :: Text
simpleStandardURL = "https://app.fossa.com/projects/haskell%2B89%2Fspectrometer/refs/branch/master/revision123"

spec :: Spec
spec = do
  let apiOpts = ApiOpts [uri|https://app.fossa.com/|] $ ApiKey ""
  describe "SAML URL builder" $ do
    it "should render simple locators" $ do
      let locator = Locator "fetcher123" "project123" $ Just "revision123"
          org = Just $ Organization 1 True
          revision = ProjectRevision "" "not this revision" $ Just "master123"

      getBuildURLWithOrg org revision apiOpts locator `shouldBe` simpleSamlPath
    
    it "should render git@ locators" $ do
      let locator = Locator "fetcher@123/abc" "git@github.com/user/repo" $ Just "revision@123/abc"
          org = Just $ Organization 103 True
          revision = ProjectRevision "not this project name" "not this revision" $ Just "weird--branch"
      
      getBuildURLWithOrg org revision apiOpts locator `shouldBe` gitSamlPath
    
    it "should render full url correctly" $ do
      let locator = Locator "a" "b" $ Just "c"
          org = Just $ Organization 33 True
          revision = ProjectRevision "" "not this revision" $ Just "master"
      
      getBuildURLWithOrg org revision apiOpts locator `shouldBe` fullSamlURL
  
  describe "Standard URL Builder" $ do
    it "should render simple links" $ do
      let locator = Locator "haskell" "89/spectrometer" $ Just "revision123"
          revision = ProjectRevision "" "not this revision" $ Just "master"
      
      getBuildURLWithOrg Nothing revision apiOpts locator `shouldBe` simpleStandardURL
