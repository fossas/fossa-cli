{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Fossa.API.BuildWaitSpec (spec) where

import App.Fossa.API.BuildWait (waitForBuild, waitForScanCompletion)
import Control.Algebra (Has)
import Control.Effect.FossaApiClient (FossaApiClient, FossaApiClientF (..), getOrganization)
import Control.Effect.Lift (Lift)
import Control.Effect.State (State, get, put)
import Control.Carrier.State.Strict (runState, StateC)
import Control.Timeout (Duration (Seconds), timeout')
import Data.Monoid (First (First, getFirst))
import Fossa.API.Types (Project (projectIsMonorepo), ScanResponse (responseScanStatus), Issues, Organization)
import Test.Effect (assertNotCalled, it', withMockApi, shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, it, pending, Expectation)
import Data.Kind (Type)
import Control.Monad (void)
import Control.Carrier.Simple (interpretState, interpret)
import Data.Type.Equality qualified as TE

mockApi :: (Has (Lift IO) sig m) => FossaApiClientF a -> m a
mockApi (GetIssues revision) = pure Fixtures.emptyIssues
mockApi (GetLatestBuild revision) = pure Fixtures.build
mockApi (GetLatestScan locator revision) = pure Fixtures.scanResponse
mockApi (GetOrganization) = pure Fixtures.organization
mockApi (GetProject revision) = pure Fixtures.project
mockApi (GetScan locator scanId) = pure Fixtures.scanResponse
mockApi req = assertNotCalled req


data ApiExpectation where
  GetOrganizationExpectation :: (FossaApiClientF Organization -> Bool) -> Organization -> ApiExpectation
  GetProjectExpectation :: (FossaApiClientF Project -> Bool) -> Project -> ApiExpectation

data ExpectationApplication
  = Once
  | Always

data EffectExpectation e = EffectExpectation ExpectationApplication e

expectedApi :: (Has (Lift IO) sig m, Has (State [EffectExpectation ApiExpectation]) sig m) => [EffectExpectation ApiExpectation] -> forall a. FossaApiClientF a -> m a
expectedApi [] req =
  assertNotCalled req
expectedApi es@(EffectExpectation _ (GetOrganizationExpectation pred org) : rest) req@(GetOrganization{}) =
  if pred req
  then do
    updateExpectations es
    pure org
  else expectedApi rest req
expectedApi es@(EffectExpectation _ (GetProjectExpectation pred proj) : rest) req@(GetProject{}) =
  if pred req
  then do
    updateExpectations es
    pure proj
  else expectedApi rest req
expectedApi (_ : rest) req =
  expectedApi rest req

updateExpectations [] =
  pure ()
updateExpectations (EffectExpectation Once _ : rest) =
  put rest
updateExpectations (EffectExpectation Always _ : _) =
  pure ()


expectedApiState req = do
  expectations <- get
  expectedApi expectations req

spec :: Spec
spec =
  describe "BuildWait" $ do
    describe "waitForScanCompletion" $ do
      describe "VPS projects" $ do
        -- let mockApiLocal (GetProject _) = pure $ Fixtures.project{projectIsMonorepo = True}
        --     mockApiLocal req = mockApi req
        it' "should return when the build is complete" $ do
          void . interpretState 
            [ EffectExpectation Always (GetOrganizationExpectation (const True) Fixtures.organization)
            , EffectExpectation Always (GetProjectExpectation (const True) Fixtures.project {projectIsMonorepo = True})
            ]
            expectedApiState
            . timeout' (Seconds 1)
            $ \cancel ->
              waitForScanCompletion Fixtures.projectRevision cancel
        it "should retry periodically if the build is not complete" $ do
          pending
        it "should cancel when the timeout expires" $ do
          pending
      describe "Non-VPS projects" $ do
        it "should return when the build is complete" $ do
          pending
        it "should retry periodically if the build is not complete" $ do
          pending
        it "should cancel when the timeout expires" $ do
          pending
    describe "waitForIssues" $ do
      it "should return when the issues are avilable" $ do
        pending
      it "should retry periodically if the issues are not available" $ do
        pending
      it "should cancel when the timeout expires" $ do
        pending
    describe "waitForBuild" $ do
      it "should return when the build is complete" $ do
        pending
      it "should terminate fatal if the build fails" $ do
        pending
      it "should retry periodically if the build is incomplete" $ do
        pending
      it "should cancel when the timeout expires" $ do
        pending
