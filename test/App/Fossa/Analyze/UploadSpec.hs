{-# LANGUAGE GADTs #-}

module App.Fossa.Analyze.UploadSpec (spec) where

import App.Fossa.Analyze.Upload (uploadSuccessfulAnalysis)
import App.Fossa.Config.Analyze (JsonOutput (JsonOutput))
import Control.Algebra (Has)
import Control.Carrier.Git (GitC)
import Control.Carrier.Simple (interpret)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Effect.Git (GitF (FetchGitContributors))
import Control.Monad (void)
import Data.Flag (toFlag)
import Fossa.API.Types (Project (..), UploadResponse (..))
import Srclib.Types (parseLocator)
import Test.Effect (expectError', it', shouldBe', withMockApi)
import Test.Fixtures qualified as Fixtures
import Test.Hspec (SpecWith, describe)

mockApi :: (Has Diagnostics sig m) => forall a. FossaApiClientF a -> m a
mockApi (GetProject _) = pure Fixtures.project
mockApi UploadAnalysis{} = pure Fixtures.uploadResponse
mockApi UploadContributors{} = pure ()
mockApi GetApiOpts = pure Fixtures.apiOpts
mockApi GetOrganization = pure Fixtures.organization

withGit :: (forall x. GitF x -> m x) -> GitC m a -> m a
withGit = interpret

mockGit :: Applicative m => GitF a -> m a
mockGit (FetchGitContributors{}) = pure Fixtures.contributors

spec :: SpecWith ()
spec =
  describe "uploadSuccessfulAnalysis" $ do
    it' "uploads analysis and git contributors"
      . withMockApi mockApi
      . withGit mockGit
      $ do
        locator <-
          uploadSuccessfulAnalysis
            Fixtures.baseDir
            Fixtures.projectMedata
            (toFlag (JsonOutput) False)
            Fixtures.projectRevision
            Fixtures.sourceUnits
        let expected = parseLocator $ uploadLocator Fixtures.uploadResponse
        locator `shouldBe'` expected
    -- Currently our StdOut logging just writes directly to StdOut, so this is
    -- just checking it doesn't fail.  In the future we should extract that so
    -- we can test it better.
    it' "renders JSON output when requested"
      . withMockApi mockApi
      . withGit mockGit
      $ do
        locator <-
          uploadSuccessfulAnalysis
            Fixtures.baseDir
            Fixtures.projectMedata
            (toFlag (JsonOutput) True)
            Fixtures.projectRevision
            Fixtures.sourceUnits
        let expected = parseLocator $ uploadLocator Fixtures.uploadResponse
        locator `shouldBe'` expected
    it' "aborts when uploading to a monorepo"
      . withMockApi
        ( \case
            GetProject _ -> pure $ Fixtures.project{projectIsMonorepo = True}
            _ -> fatalText "Unexpected API request: should have aborted"
        )
      . withGit mockGit
      . expectError' "This project already exists as a monorepo project."
      . void
      $ uploadSuccessfulAnalysis
        Fixtures.baseDir
        Fixtures.projectMedata
        (toFlag (JsonOutput) False)
        Fixtures.projectRevision
        Fixtures.sourceUnits
    it' "continues if fetching the project fails"
      . withMockApi
        ( \case
            GetProject{} -> fatalText "Mocked failure of GetProject"
            req -> mockApi req
        )
      . withGit mockGit
      $ do
        locator <-
          uploadSuccessfulAnalysis
            Fixtures.baseDir
            Fixtures.projectMedata
            (toFlag (JsonOutput) False)
            Fixtures.projectRevision
            Fixtures.sourceUnits
        let expected = parseLocator $ uploadLocator Fixtures.uploadResponse
        locator `shouldBe'` expected
    it' "continues if fetching contributors fails"
      . withMockApi mockApi
      . withGit (\_ -> fatalText "Mock error fetching contributors from git")
      $ do
        locator <-
          uploadSuccessfulAnalysis
            Fixtures.baseDir
            Fixtures.projectMedata
            (toFlag (JsonOutput) False)
            Fixtures.projectRevision
            Fixtures.sourceUnits
        let expected = parseLocator $ uploadLocator Fixtures.uploadResponse
        locator `shouldBe'` expected
    it' "continues if uploading contributors fails"
      . withMockApi
        ( \case
            UploadContributors{} -> fatalText "Mock error uploading contributors"
            req -> mockApi req
        )
      . withGit mockGit
      $ do
        locator <-
          uploadSuccessfulAnalysis
            Fixtures.baseDir
            Fixtures.projectMedata
            (toFlag (JsonOutput) False)
            Fixtures.projectRevision
            Fixtures.sourceUnits
        let expected = parseLocator $ uploadLocator Fixtures.uploadResponse
        locator `shouldBe'` expected
