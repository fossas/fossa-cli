module App.Fossa.VSIDepsSpec (
  spec,
) where

import App.Fossa.VSI.Types (Locator (..), SkipResolution (..), VsiRule (..), VsiRulePath (..))
import App.Fossa.VSIDeps (ruleToSourceUnit)
import Data.Set qualified as Set
import Test.Effect (it', shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)

locator :: Locator
locator =
  Locator
    { locatorFetcher = "mvn"
    , locatorProject = "something/mavenish"
    , locatorRevision = "1.2.3"
    }

spec :: Spec
spec = do
  describe "ruleToSourceUnit" $ do
    it' "should find deps" $ do
      let path = VsiRulePath "/tmp/one/two"
      let vsiRule = VsiRule path locator
      sourceUnit <- ruleToSourceUnit (SkipResolution Set.empty) vsiRule
      sourceUnit `shouldBe'` Fixtures.vsiSourceUnit
