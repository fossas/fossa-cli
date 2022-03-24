{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module App.Fossa.API.BuildWaitSpec (spec) where

import App.Fossa.API.BuildWait (waitForBuild, waitForScanCompletion)
import Control.Algebra (Has)
import Control.Effect.FossaApiClient (FossaApiClient, FossaApiClientF (..), getOrganization)
import Control.Effect.Lift (Lift)
import Control.Effect.State (State, get, put)
import Control.Carrier.State.Strict (runState, StateC)
import Control.Timeout (Duration (Seconds), timeout')
import Data.Monoid (First (First, getFirst))
import Fossa.API.Types (Project (projectIsMonorepo), ScanResponse (responseScanStatus))
import Test.Effect (assertNotCalled, it', withMockApi, shouldBe')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe, it, pending)
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

data Expectations eff :: [Type] -> Type where
  NoExpectations :: Expectations eff '[]
  Always :: (eff a -> Bool) -> a -> Expectations eff xs -> Expectations eff (a ': xs)
  Once :: (eff a -> Bool) -> a -> Expectations eff xs -> Expectations eff (a ': xs)
  Skip :: Expectations eff xs -> Expectations eff (a ': xs)
  InOrder :: (eff a -> Bool) -> [a] -> Expectations eff xs -> Expectations eff (a ': xs)

class FindExpectation eff a xs where
  findExpectation :: eff a -> Expectations eff xs -> Maybe (a, Expectations eff xs)

instance FindExpectation eff a '[] where
  findExpectation _ _ = Nothing

instance {-# OVERLAPPING #-} FindExpectation eff a xs => FindExpectation eff a (a ': xs) where
  findExpectation req (Once pred resp rest) =
    if pred req
      then Just (resp, Skip rest)
      else fmap (Once pred resp) <$> findExpectation req rest
  findExpectation req expectations@(Always pred resp rest) =
    if pred req
      then Just (resp, expectations)
      else fmap (Once pred resp) <$> findExpectation req rest
  findExpectation req (Skip rest) =
      fmap Skip <$> findExpectation req rest
  findExpectation req (InOrder _ [] rest) =
      fmap Skip <$> findExpectation req rest
  findExpectation req (InOrder pred (resp:resps) rest) =
    if pred req
      then Just (resp, InOrder pred resps rest)
      else fmap (InOrder pred (resp:resps)) <$> findExpectation req rest

instance {-# OVERLAPPABLE #-} ((a TE.== b) ~ 'False, FindExpectation eff a xs) => FindExpectation eff a (b ': xs) where
  findExpectation req (Once pred resp rest) =
    fmap (Once pred resp) <$> findExpectation req rest
  findExpectation req (Always pred resp rest) =
    fmap (Always pred resp) <$> findExpectation req rest
  findExpectation req (Skip rest) =
    fmap (Skip) <$> findExpectation req rest
  findExpectation req (InOrder pred resps rest) =
    fmap (InOrder pred resps) <$> findExpectation req rest

expectedApi ::
  ( Has (Lift IO) sig m
  , FindExpectation FossaApiClientF a xs 
  ) => FossaApiClientF a -> StateC (Expectations FossaApiClientF xs) m a
expectedApi req = do
  expectations :: Expectations FossaApiClientF xs <- get
  case findExpectation req expectations of
    Just (resp, expectations') -> do
      put expectations'
      pure resp
    Nothing -> assertNotCalled req

-- stateWrapper :: (Has (State a) sig m) => (x -> a -> m (a, b)) -> x -> StateC a m b
-- stateWrapper f req = do
--   st :: a <- get
--   (st', res) <- lift $ f req st
--   put st'
--   pure res

statelessExpectedApi ::
  Has (Lift IO) sig m => forall a. FindExpectation FossaApiClientF a xs => Expectations FossaApiClientF xs -> FossaApiClientF a ->  m a
statelessExpectedApi expectations req = do
  case findExpectation req expectations of
    Just (resp, _) ->
      pure resp
    Nothing -> assertNotCalled req

spec :: Spec
spec =
  describe "BuildWait" $ do
    describe "waitForScanCompletion" $ do
      describe "VPS projects" $ do
        let mockApiLocal (GetProject _) = pure $ Fixtures.project{projectIsMonorepo = True}
            mockApiLocal req = mockApi req
        it' "should return when the build is complete" $ do
          let expectations = Always (\case
                  GetOrganization -> True
                  _ -> False
                ) Fixtures.organization NoExpectations
          void . interpret (statelessExpectedApi expectations)
          -- . interpretState expectations expectedApi
            -- ( \case
            --     GetScan{} -> pure $ Fixtures.scanResponse{responseScanStatus = Just "AVAILABLE"}
            --     req -> mockApiLocal req
            -- )
            -- . timeout' (Seconds 1)
            -- $ \cancel ->
            --   waitForScanCompletion Fixtures.projectRevision cancel
            $ do
              org <- getOrganization
              org `shouldBe'` Fixtures.organization
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
