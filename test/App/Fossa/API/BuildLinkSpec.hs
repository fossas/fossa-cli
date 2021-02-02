module App.Fossa.API.BuildLinkSpec (spec) where

import Test.Hspec
import App.Fossa.API.BuildLink
import Srclib.Types (Locator(Locator))
import App.Fossa.FossaAPIV1 (Organization(Organization))
import App.Types (ProjectRevision(ProjectRevision))
import Data.Text (Text)

simpleSamlPath :: Text
simpleSamlPath = "account/saml/1?next=%2Fprojects%2Ffetcher123%252Bproject123%2Frefs%2Fbranch%2Fmaster123%2Frevision123"

-- | Note the differences here between '%2F' and '%252F'.  The percent sign is re-encoded so that it's properly handled on the next redirect.
gitSamlPath :: Text
gitSamlPath = "account/saml/103?next=%2Fprojects%2Ffetcher%2540123%252Fabc%252Bgit%2540github.com%252Fuser%252Frepo%2Frefs%2Fbranch%2Fweird--branch%2Frevision%2540123%252Fabc"

spec :: Spec
spec = do
  describe "SAML URL builder" $ do
    it "should render simple locators" $ do
      let locator = Locator "fetcher123" "project123" $ Just "revision123"
          org = Organization 1 False -- Bool is ignored at this point
          revision = ProjectRevision "" "not this revision" $ Just "master123"

      samlUrlPath org locator revision `shouldBe` simpleSamlPath
    
    it "should render git@ locators" $ do
      let locator = Locator "fetcher@123/abc" "git@github.com/user/repo" $ Just "revision@123/abc"
          org = Organization 103 True
          revision = ProjectRevision "not this project name" "not this revision" $ Just "weird--branch"
      
      samlUrlPath org locator revision `shouldBe` gitSamlPath