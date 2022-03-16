{-# LANGUAGE GADTs #-}

module App.Fossa.Analyze.UploadSpec (spec) where

import App.Fossa.Analyze.Upload (dieOnMonorepoUpload)
import App.Types (ProjectRevision (..))
import Control.Carrier.Simple
import Control.Effect.FossaAPIClient
import Fossa.API.Types (ApiKey (..), ApiOpts (..), Project (..))
import Test.Effect
import Test.Hspec

testRevision :: ProjectRevision
testRevision =
  ProjectRevision
    { projectName = "TestProjectName"
    , projectRevision = "TestProjectRevision"
    , projectBranch = Just "TestProjectBranch"
    }

apiOpts :: ApiOpts
apiOpts =
  ApiOpts
    { apiOptsUri = Nothing
    , apiOptsApiKey = ApiKey "TestKey"
    }

spec :: SpecWith ()
spec = describe "AnalysisUpload" $ do
  describe "dieOnMonorepoUpload" $ do
    it' "succeeds if the result is not a monorepo" $
      withMockApi
        ( \(GetProject _ _) ->
            pure
              Project
                { projectId = "TestID"
                , projectTitle = "Test Project"
                , projectIsMonorepo = False
                }
        )
        $ dieOnMonorepoUpload apiOpts testRevision
    it' "fails if the result is a monorepo" $
      withMockApi
        ( \(GetProject _ _) ->
            pure
              Project
                { projectId = "TestID"
                , projectTitle = "Test Project"
                , projectIsMonorepo = True
                }
        )
        $ expectFails' $ dieOnMonorepoUpload apiOpts testRevision

withMockApi :: (forall x. FossaAPIClientF x -> m x) -> SimpleC FossaAPIClientF m a -> m a
withMockApi = interpret
