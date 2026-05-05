module Fossa.API.CoreTypesSpec (spec) where

import Data.Aeson (Value (Object, String), toJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Fossa.API.CoreTypes (UpdateProjectRequest (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "UpdateProjectRequest ToJSON" $ do
  it "omits Nothing fields rather than emitting JSON null" $ do
    let req =
          UpdateProjectRequest
            { updateProjectTitle = Nothing
            , updateProjectUrl = Nothing
            , updateProjectIssueTrackerIds = Nothing
            , updateProjectLabelIds = Nothing
            , updateProjectPolicyId = Just 7
            , updateProjectDefaultBranch = Nothing
            }
        expected = Object $ KeyMap.fromList [("policyId", Aeson.Number 7)]
    toJSON req `shouldBe` expected

  it "includes every Just field with its JSON-encoded value" $ do
    let req =
          UpdateProjectRequest
            { updateProjectTitle = Just "my-title"
            , updateProjectUrl = Just "https://example.com"
            , updateProjectIssueTrackerIds = Just ["JIRA-1"]
            , updateProjectLabelIds = Just [1, 2]
            , updateProjectPolicyId = Just 7
            , updateProjectDefaultBranch = Just "main"
            }
        expected =
          Object $
            KeyMap.fromList
              [ ("title", String "my-title")
              , ("url", String "https://example.com")
              , ("issueTrackerProjectIds", toJSON (["JIRA-1"] :: [Text]))
              , ("labels", toJSON ([1, 2] :: [Int]))
              , ("policyId", Aeson.Number 7)
              , ("default_branch", String "main")
              ]
    toJSON req `shouldBe` expected
