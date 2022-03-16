{-# LANGUAGE GADTs #-}

module App.Fossa.Analyze.UploadSpec (spec) where

import App.Fossa.Analyze.Upload (dieOnMonorepoUpload)
import App.Types (ProjectRevision (..))
import Control.Carrier.Simple ( interpret, SimpleC )
import Control.Effect.FossaApiClient ( FossaApiClientF(..) )
import Fossa.API.Types (Project (..))
import Test.Effect ( it', expectFatal' )
import Test.Hspec ( describe, SpecWith )

testRevision :: ProjectRevision
testRevision =
  ProjectRevision
    { projectName = "TestProjectName"
    , projectRevision = "TestProjectRevision"
    , projectBranch = Just "TestProjectBranch"
    }

spec :: SpecWith ()
spec = describe "AnalysisUpload" $
  describe "dieOnMonorepoUpload" $ do
    it' "succeeds if the result is not a monorepo" $
      withMockApi
        ( \(GetProject _) ->
            pure
              Project
                { projectId = "TestID"
                , projectTitle = "Test Project"
                , projectIsMonorepo = False
                }
        )
        $ dieOnMonorepoUpload testRevision
    it' "fails if the result is a monorepo"
      . withMockApi
        ( \(GetProject _) ->
            pure
              Project
                { projectId = "TestID"
                , projectTitle = "Test Project"
                , projectIsMonorepo = True
                }
        )
      . expectFatal'
      $ dieOnMonorepoUpload testRevision

withMockApi :: (forall x. FossaApiClientF x -> m x) -> SimpleC FossaApiClientF m a -> m a
withMockApi = interpret
