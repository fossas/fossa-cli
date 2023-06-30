{-# LANGUAGE QuasiQuotes #-}

module App.Fossa.VSI.TypesSpec (spec) where

import App.Fossa.VSI.Types (Locator (..), VsiExportedInferencesBody (VsiExportedInferencesBody), VsiFilePath (..), VsiInference (VsiInference), VsiRule (VsiRule), VsiRulePath (..), generateRules)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Either (isLeft)
import Data.Map qualified as Map
import Test.Hspec (Spec, describe, it, shouldBe, shouldMatchList, shouldSatisfy)
import Test.Hspec.Core.Spec (focus)
import Text.RawString.QQ

inferencesBody :: BS.ByteString
inferencesBody =
  [r|
{
  "InferencesByFilePath": {
    "/foo/bar.h": {
      "RawSha256": "YmIwMTg2NTNlOTVlY2U5M2VmMDYwMTQ3YjA0ZjZhYzRkZjhlMzFhZDc1OWFjYmExZWJmMjIwZDVjZTJlM2ZkZQ==",
      "ComponentID": "0f4ba6a8-5b3f-436f-8c36-828e7375aef7",
      "Locator": "git+github.com/facebook/folly$v2016.08.08.00",
      "Confidence": 1
    }
  }
}
|]

emptyLocatorInference :: BS.ByteString
emptyLocatorInference =
  [r|
{
  "InferencesByFilePath": {
    "/foo/bar.h": {
      "RawSha256": "",
      "ComponentID": "",
      "Locator": "",
      "Confidence": 1
    }
  }
}
|]

invalidLocatorInference :: BS.ByteString
invalidLocatorInference =
  [r|
{
  "InferencesByFilePath": {
    "/foo/bar.h": {
      "RawSha256": "",
      "ComponentID": "",
      "Locator": "bad-locator",
      "Confidence": 1
    }
  }
}
|]

expectedEmptyLocatorInference :: VsiExportedInferencesBody
expectedEmptyLocatorInference =
  VsiExportedInferencesBody $
    Map.fromList
      [
        ( VsiFilePath "/foo/bar.h"
        , VsiInference Nothing
        )
      ]

expectedSingleInference :: VsiExportedInferencesBody
expectedSingleInference =
  VsiExportedInferencesBody $ Map.fromList singleInference

singleInference :: [(VsiFilePath, VsiInference)]
singleInference =
  [
    ( VsiFilePath "/foo/bar.h"
    , VsiInference . Just $ Locator "git" "github.com/facebook/folly" "v2016.08.08.00"
    )
  ]

singleRuleExpected :: [VsiRule]
singleRuleExpected = [VsiRule (VsiRulePath "/foo") (Locator "git" "github.com/facebook/folly" "v2016.08.08.00")]

commonPrefixInferences :: [(VsiFilePath, VsiInference)]
commonPrefixInferences =
  (VsiFilePath "/foo/bar/baz.c", VsiInference . Just $ Locator "git" "github.com/facebook/folly" "v2016.08.08.00") : singleInference

multipleInferences :: [(VsiFilePath, VsiInference)]
multipleInferences =
  (VsiFilePath "/otherProject/Readme.md", VsiInference . Just $ Locator "git" "github.com/otherProject" "2.0.0")
    : (VsiFilePath "/baz/hello.c", VsiInference . Just $ Locator "git" "github.com/someProject" "1.0.0")
    : commonPrefixInferences

multipleRulesExpected :: [VsiRule]
multipleRulesExpected =
  VsiRule (VsiRulePath "/otherProject") (Locator "git" "github.com/otherProject" "2.0.0")
    : VsiRule (VsiRulePath "/baz") (Locator "git" "github.com/someProject" "1.0.0")
    : singleRuleExpected

nestedProjectInferences :: [(VsiFilePath, VsiInference)]
nestedProjectInferences =
  ( VsiFilePath "/foo/bar/g.c"
  , VsiInference . Just $ Locator "git" "github.com/facebook/follyNested" "1.0.0"
  )
    : singleInference

nestedProjectRulesExpected :: [VsiRule]
nestedProjectRulesExpected =
  VsiRule (VsiRulePath "/foo/bar") (Locator "git" "github.com/facebook/follyNested" "1.0.0")
    : singleRuleExpected

vsiTypesSpec :: Spec
vsiTypesSpec = describe "VSI Types" $ do
  it "Parses a VsiExportedInferencesBody" $ do
    let body = eitherDecode inferencesBody
    body `shouldBe` Right expectedSingleInference
  it "Accepts an empty string locator" $ do
    let body = eitherDecode emptyLocatorInference
    body `shouldBe` Right expectedEmptyLocatorInference
  it "Will not parse with an invalid locator" $ do
    let body = eitherDecode invalidLocatorInference :: Either String VsiExportedInferencesBody
    body `shouldSatisfy` isLeft

generateRulesSpec :: Spec
generateRulesSpec = describe "generateRules" $ do
  it "Generates a rule correctly" $
    generateRules expectedSingleInference `shouldBe` singleRuleExpected
  it "Reduces rules to common prefixes" $
    generateRules' commonPrefixInferences `shouldBe` singleRuleExpected
  it "Reports multiple rules for a project" $
    generateRules' multipleInferences `shouldMatchList` multipleRulesExpected
  it "Reports distinct locators for nested projects" $
    generateRules' nestedProjectInferences `shouldMatchList` nestedProjectRulesExpected
  where
    generateRules' :: [(VsiFilePath, VsiInference)] -> [VsiRule]
    generateRules' = generateRules . VsiExportedInferencesBody . Map.fromList

spec :: Spec
spec = do
  vsiTypesSpec
  generateRulesSpec
