module Fossa.API.TypesSpec (spec) where

import Data.Aeson (FromJSON, ToJSON, fromJSON, toJSON)
import Data.Text (Text)
import Fossa.API.Types (Issue (..), IssueRevision (..), IssueRevisionSummary (..), IssueRevisionTarget (..), IssueRule (..), IssueType (..), Issues (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog
import Prelude

spec :: Spec
spec = do
  describe "Issues ToJSON/FromJSON instances" $ do
    it "are roundtrippable" $
      hedgehog $ do
        issues <- forAll genIssues
        roundtripJson issues

genIssues :: Gen Issues
genIssues =
  Issues
    <$> Gen.int (Range.linear minBound maxBound)
    <*> Gen.list (Range.linear 0 100) genIssue
    <*> arbitraryText
    <*> Gen.maybe genIssueRevisionSummary

genIssueRevisionSummary :: Gen IssueRevisionSummary
genIssueRevisionSummary =
  IssueRevisionSummary
    <$> genIssueRevision
    <*> (Gen.maybe $ Gen.list (Range.linear 0 100) genIssueRevisionTargets)

genIssueRevision :: Gen IssueRevision
genIssueRevision =
  IssueRevision
    <$> arbitraryText
    <*> arbitraryText
    <*> Gen.maybe (Gen.bool)

genIssueRevisionTargets :: Gen IssueRevisionTarget
genIssueRevisionTargets =
  IssueRevisionTarget
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
genIssueRule = IssueRule <$> Gen.maybe arbitraryText

arbitraryText :: Gen Text
arbitraryText = Gen.text (Range.linear 0 100) Gen.unicodeAll

roundtripJson :: (MonadTest m, Show a, Eq a, ToJSON a, FromJSON a) => a -> m ()
roundtripJson a = tripping a toJSON fromJSON
