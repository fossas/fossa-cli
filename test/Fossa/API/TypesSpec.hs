module Fossa.API.TypesSpec (spec) where

import Data.Aeson (FromJSON, ToJSON, Value (Object), fromJSON, toJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Fossa.API.Types (Issue (..), IssueRule (..), IssueSummaryRevision (..), IssueSummaryTarget (..), IssueType (..), Issues (..), IssuesSummary (..), Organization (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog
import Prelude

spec :: Spec
spec = do
  organizationParsingSpec
  describe "Issues ToJSON/FromJSON instances" $ do
    it "are roundtrippable" $
      hedgehog $ do
        issues <- forAll genIssues
        roundtripJson issues

organizationParsingSpec :: Spec
organizationParsingSpec = describe "Organization JSON parsing" $ do
  it "defaults supportsGitBackedCargoLocators to False when field is absent" $ do
    let json = Object $ KeyMap.fromList [("organizationId", Aeson.Number 1)]
    case fromJSON json of
      Aeson.Success org -> orgSupportsGitBackedCargoLocators org `shouldBe` False
      Aeson.Error err -> expectationFailure err

  it "parses supportsGitBackedCargoLocators when explicitly True" $ do
    let json =
          Object $
            KeyMap.fromList
              [ ("organizationId", Aeson.Number 1)
              , ("supportsGitBackedCargoLocators", Aeson.Bool True)
              ]
    case fromJSON json of
      Aeson.Success org -> orgSupportsGitBackedCargoLocators org `shouldBe` True
      Aeson.Error err -> expectationFailure err

  it "parses supportsGitBackedCargoLocators when explicitly False" $ do
    let json =
          Object $
            KeyMap.fromList
              [ ("organizationId", Aeson.Number 1)
              , ("supportsGitBackedCargoLocators", Aeson.Bool False)
              ]
    case fromJSON json of
      Aeson.Success org -> orgSupportsGitBackedCargoLocators org `shouldBe` False
      Aeson.Error err -> expectationFailure err

genIssues :: Gen Issues
genIssues =
  Issues
    <$> Gen.int (Range.linear minBound maxBound)
    <*> Gen.list (Range.linear 0 100) genIssue
    <*> arbitraryText
    <*> Gen.maybe genIssueRevisionSummary

genIssueRevisionSummary :: Gen IssuesSummary
genIssueRevisionSummary =
  IssuesSummary
    <$> genIssueRevision
    <*> Gen.list (Range.linear 0 100) genIssueRevisionTargets

genIssueRevision :: Gen IssueSummaryRevision
genIssueRevision =
  IssueSummaryRevision
    <$> arbitraryText
    <*> arbitraryText
    <*> Gen.maybe (Gen.bool)

genIssueRevisionTargets :: Gen IssueSummaryTarget
genIssueRevisionTargets =
  IssueSummaryTarget
    <$> arbitraryText
    <*> Gen.list (Range.linear 0 5) arbitraryText

genIssue :: Gen Issue
genIssue =
  Issue
    <$> Gen.int (Range.linear minBound maxBound)
    <*> Gen.maybe arbitraryText
    <*> Gen.bool
    <*> arbitraryText
    <*> genIssueType
    <*> Gen.maybe genIssueRule
    <*> Gen.maybe arbitraryText
    <*> Gen.maybe arbitraryText
    <*> Gen.maybe arbitraryText
    <*> Gen.maybe arbitraryText

genIssueType :: Gen IssueType
genIssueType =
  Gen.element
    [ IssuePolicyConflict
    , IssuePolicyFlag
    , IssueVulnerability
    , IssueUnlicensedDependency
    , IssueOutdatedDependency
    , IssueOther "something else"
    ]

genIssueRule :: Gen IssueRule
genIssueRule =
  IssueRule <$> Gen.maybe (Gen.int (Range.linear 0 100))

arbitraryText :: Gen Text
arbitraryText = Gen.text (Range.linear 0 100) Gen.unicodeAll

roundtripJson :: (MonadTest m, Show a, Eq a, ToJSON a, FromJSON a) => a -> m ()
roundtripJson a = tripping a toJSON fromJSON
