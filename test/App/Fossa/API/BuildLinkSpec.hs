{-# LANGUAGE QuasiQuotes #-}

module App.Fossa.API.BuildLinkSpec (spec) where

import App.Fossa.API.BuildLink
import App.Fossa.FossaAPIV1 (Organization (Organization))
import App.Types (ProjectRevision (ProjectRevision))
import Control.Carrier.Diagnostics (DiagnosticsC, logDiagnostic)
import Control.Monad (join)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Text (Text)
import Effect.Logger (IgnoreLoggerC, ignoreLogger)
import Fossa.API.Types
import Srclib.Types (Locator (Locator))
import Test.Hspec
import Text.URI.QQ

simpleSamlPath :: Text
simpleSamlPath = "https://app.fossa.com/account/saml/1?next=/projects/fetcher123%2bproject123/refs/branch/master123/revision123"

-- | Note the differences here between '%2F' and '%252F'.  The percent sign is re-encoded so that it's properly handled on the next redirect.
gitSamlPath :: Text
gitSamlPath = "https://app.fossa.com/account/saml/103?next=/projects/fetcher@123%252fabc%2bgit@github.com%252fuser%252frepo/refs/branch/weird--branch/revision@123%252fabc"

fullSamlURL :: Text
fullSamlURL = "https://app.fossa.com/account/saml/33?next=/projects/a%2bb/refs/branch/master/c"

simpleStandardURL :: Text
simpleStandardURL = "https://app.fossa.com/projects/haskell+89%2fspectrometer/refs/branch/master/revision123"

stripDiag :: DiagnosticsC (IgnoreLoggerC Maybe) a -> Maybe a
stripDiag = join . ignoreLogger . logDiagnostic

spec :: Spec
spec = do
  let apiOpts = ApiOpts [uri|https://app.fossa.com/|] $ ApiKey ""
  describe "SAML URL builder" $ do
    it "should render simple locators" $ do
      let locator = Locator "fetcher123" "project123" $ Just "revision123"
          org = Just $ Organization 1 True
          revision = ProjectRevision "" "not this revision" $ Just "master123"
          -- Loggers and Diagnostics modify monads, so we need a no-op monad
          actual = runIdentity $ ignoreLogger $ logDiagnostic $ getBuildURLWithOrg org revision apiOpts locator

      actual `shouldBe` Just simpleSamlPath

    it "should render git@ locators" $ do
      let locator = Locator "fetcher@123/abc" "git@github.com/user/repo" $ Just "revision@123/abc"
          org = Just $ Organization 103 True
          revision = ProjectRevision "not this project name" "not this revision" $ Just "weird--branch"
          actual = stripDiag $ getBuildURLWithOrg org revision apiOpts locator

      actual `shouldBe` Just gitSamlPath

    it "should render full url correctly" $ do
      let locator = Locator "a" "b" $ Just "c"
          org = Just $ Organization 33 True
          revision = ProjectRevision "" "not this revision" $ Just "master"
          actual = stripDiag $ getBuildURLWithOrg org revision apiOpts locator

      actual `shouldBe` Just fullSamlURL

  describe "Standard URL Builder" $ do
    it "should render simple links" $ do
      let locator = Locator "haskell" "89/spectrometer" $ Just "revision123"
          revision = ProjectRevision "" "not this revision" $ Just "master"
          actual = stripDiag $ getBuildURLWithOrg Nothing revision apiOpts locator

      actual `shouldBe` Just simpleStandardURL
