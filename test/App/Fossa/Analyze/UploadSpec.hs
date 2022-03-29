{-# LANGUAGE GADTs #-}

module App.Fossa.Analyze.UploadSpec (spec) where

import App.Fossa.Analyze.Upload (uploadSuccessfulAnalysis)
import App.Fossa.Config.Analyze (JsonOutput (JsonOutput))
import Control.Algebra (Has)
import Control.Carrier.Git (GitC)
import Control.Carrier.Simple (interpret)
import Control.Effect.Diagnostics (fatalText)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Effect.Git (GitF (FetchGitContributors))
import Control.Effect.Lift (Lift)
import Data.Flag (toFlag)
import Fossa.API.Types (Project (..), UploadResponse (..))
import Srclib.Types (Locator)
import Test.Effect (assertNotCalled, expectFatal', it', shouldBe', withMockApi)
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)
import Test.Hspec.Core.Spec (runIO)
import Test.MockApi (
  ApiExpectation,
  alwaysReturns,
  fails,
  returnsOnce,
  runWithApiExpectations,
 )
import Control.Monad (void)

-- | Mock API for this spec that returns fixture data.
-- This is here instead of using expectations because the expecctations
-- currently don't support failure which we need for some of these tests.
mockApi :: (Has (Lift IO) sig m) => forall a. FossaApiClientF a -> m a
mockApi (GetProject _) = pure Fixtures.project
mockApi UploadAnalysis{} = pure Fixtures.uploadResponse
mockApi UploadContributors{} = pure ()
mockApi GetApiOpts = pure Fixtures.apiOpts
mockApi GetOrganization = pure Fixtures.organization
mockApi req = assertNotCalled req

withGit :: (forall x. GitF x -> m x) -> GitC m a -> m a
withGit = interpret

mockGit :: Applicative m => GitF a -> m a
mockGit (FetchGitContributors{}) = pure Fixtures.contributors

expectedLocator :: Locator
expectedLocator = uploadLocator Fixtures.uploadResponse

expectGetSuccess :: [ApiExpectation]
expectGetSuccess =
  [ GetProject Fixtures.projectRevision `alwaysReturns` Fixtures.project
  , GetOrganization `alwaysReturns` Fixtures.organization
  , GetApiOpts `alwaysReturns` Fixtures.apiOpts
  ]

expectAnalysisUploadSuccess :: ApiExpectation
expectAnalysisUploadSuccess = UploadAnalysis Fixtures.projectRevision Fixtures.projectMetadata Fixtures.sourceUnits `alwaysReturns` Fixtures.uploadResponse

expectContributorUploadSucces :: ApiExpectation
expectContributorUploadSucces =
  UploadContributors expectedLocator Fixtures.contributors `alwaysReturns` ()

spec :: Spec
spec =
  describe "uploadSuccessfulAnalysis" $ do
    baseDir <- runIO Fixtures.baseDir
    it' "uploads analysis and git contributors"
      . runWithApiExpectations (expectAnalysisUploadSuccess : expectContributorUploadSucces : expectGetSuccess)
      . withGit mockGit
      $ do
        locator <-
          uploadSuccessfulAnalysis
            baseDir
            Fixtures.projectMetadata
            (toFlag (JsonOutput) False)
            Fixtures.projectRevision
            Fixtures.sourceUnits
        locator `shouldBe'` expectedLocator
    -- Currently our StdOut logging just writes directly to StdOut, so this is
    -- just checking it doesn't fail.  In the future we should extract that so
    -- we can test it better.
    it' "renders JSON output when requested"
      . runWithApiExpectations (expectAnalysisUploadSuccess : expectContributorUploadSucces : expectGetSuccess)
      . withGit mockGit
      $ do
        locator <-
          uploadSuccessfulAnalysis
            baseDir
            Fixtures.projectMetadata
            (toFlag (JsonOutput) True)
            Fixtures.projectRevision
            Fixtures.sourceUnits
        locator `shouldBe'` expectedLocator
    it' "aborts when uploading to a monorepo"
      . expectFatal'
      . runWithApiExpectations
        [ GetProject Fixtures.projectRevision `returnsOnce` Fixtures.project{projectIsMonorepo = True}
        ]
      . withGit mockGit
      $ uploadSuccessfulAnalysis
        baseDir
        Fixtures.projectMetadata
        (toFlag (JsonOutput) False)
        Fixtures.projectRevision
        Fixtures.sourceUnits
    it' "continues if fetching the project fails"
      . runWithApiExpectations
        [ GetProject Fixtures.projectRevision `fails` "Mocked failure fetching project"
        , expectAnalysisUploadSuccess 
        , expectContributorUploadSucces
        , GetOrganization `alwaysReturns` Fixtures.organization
        , GetApiOpts `alwaysReturns` Fixtures.apiOpts
        ]
        
      . withGit mockGit
      $ do
        locator <-
          uploadSuccessfulAnalysis
            baseDir
            Fixtures.projectMetadata
            (toFlag (JsonOutput) False)
            Fixtures.projectRevision
            Fixtures.sourceUnits
        locator `shouldBe'` expectedLocator
    it' "continues if fetching contributors fails"
      . runWithApiExpectations expectGetSuccess
      . withGit (\_ -> fatalText "Mocked failure of fetching contributors from git")
      $ do
        locator <-
          uploadSuccessfulAnalysis
            baseDir
            Fixtures.projectMetadata
            (toFlag (JsonOutput) False)
            Fixtures.projectRevision
            Fixtures.sourceUnits
        locator `shouldBe'` expectedLocator
    it' "continues if uploading contributors fails"
      . runWithApiExpectations
        ( UploadContributors expectedLocator Fixtures.contributors `fails` "Mocked failure uploading contributors"
        : expectAnalysisUploadSuccess
        : expectGetSuccess
        )
      . withGit mockGit
      $ do
        locator <-
          uploadSuccessfulAnalysis
            baseDir
            Fixtures.projectMetadata
            (toFlag (JsonOutput) False)
            Fixtures.projectRevision
            Fixtures.sourceUnits
        locator `shouldBe'` expectedLocator
