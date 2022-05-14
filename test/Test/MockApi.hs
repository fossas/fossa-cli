{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A framework for Mocking API effects for testing
--
-- This system resolves around setting expectations.  An expectation represents a
-- request we expect to be made and the result of that request.  Additionally, the
-- expectation may be that the request is made only once or many times.
--
-- The request must be a @FossaApiClientF@ and must exactly be the request
-- expected.  This is due to some limitations on matching types at run time.
-- Expectations are matched in the order they are set up.  An assertion failure
-- will be raised if a request is made that doesn't have a matching expectation.
--
-- The result can either be a return value or to die with a fatal diagnostic
-- error.
--
-- Set up expectations using the `returnsOnce`, `alwaysReturns` and `fails`
-- helpers.
--
-- @
--     -- Set up basic expectation
--     GetOrganization `alwaysReturns` Fixtures.organization
--     GetProject revision `returnsOnce` Fixtures.project
--
--     -- Expect get issues to be called and fail, then return successfully when
--     -- called again.
--     GetIssues revision `fails` "Mock API error"
--     GetIssues revision `returnsOnce` Fixtures.issues
-- @
--
-- An expectation is considered satisfied if it had a limited number of invocations
-- and it has been matched that many times.  If any expectations are unsatisfied at
-- the end of the test, an test assertion failure will be raised.
--
-- See /docs/contributing/api-mocking.md for more information.
module Test.MockApi (
  ApiExpectation,
  FossaApiClientMockC,
  MockApi (..),
  MockApiC (runMockApiC),
  alwaysReturns,
  assertAllSatisfied,
  fails,
  returnsOnce,
  runMockApi,
  runApiWithMock,
) where

import Control.Algebra (Algebra (..), Has, send, type (:+:) (..))
import Control.Carrier.Simple (SimpleC, interpret)
import Control.Carrier.State.Strict (StateC (StateC), evalState)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.State (State, get, modify, put)
import Control.Monad (guard)
import Control.Monad.Trans (MonadIO)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Text (Text)
import Test.HUnit (assertFailure)

data MockApi (m :: Type -> Type) a where
  MockApiOnce :: FossaApiClientF a -> a -> MockApi m ()
  MockApiAlways :: FossaApiClientF a -> a -> MockApi m ()
  MockApiFails :: FossaApiClientF a -> Text -> MockApi m ()
  MockApiRunExpectations :: FossaApiClientF a -> MockApi m (Maybe (ApiResult a))
  AssertUnexpectedCall :: FossaApiClientF a -> MockApi m a
  AssertAllSatisfied :: MockApi m ()

-- | Specifies how to handle repetition of a request
data ExpectationRepetition
  = Once
  | Always
  deriving (Eq, Ord, Show)

-- | The result of a call can be either a value or a diagnostic failure
newtype ApiResult a = ApiResult (Either ApiFail a)

newtype ApiFail = ApiFail {unApiFail :: Text}

-- | An expectation of an API call made up of the request and response.
data ApiExpectation where
  ApiExpectation :: ExpectationRepetition -> FossaApiClientF a -> ApiResult a -> ApiExpectation

-- | Create an expectation that will only be satisfied once.
returnsOnce :: Has MockApi sig m => FossaApiClientF a -> a -> m ()
returnsOnce req resp =
  -- ApiExpectation Once req (Return resp)
  send $ MockApiOnce req resp

-- | Create an expectation that can be satisfied multiple times.
alwaysReturns :: Has MockApi sig m => FossaApiClientF a -> a -> m ()
alwaysReturns req resp =
  -- ApiExpectation Once req (Return resp)
  send $ MockApiAlways req resp

-- | Fail with a fatal diagnostic error
fails :: Has MockApi sig m => FossaApiClientF a -> Text -> m ()
fails req msg =
  -- ApiExpectation Once req (Die msg)
  send $ MockApiFails req msg

-- | Run a request against the expectations
runExpectations :: Has MockApi sig m => FossaApiClientF a -> m (Maybe (ApiResult a))
runExpectations =
  send . MockApiRunExpectations

-- | Assert that this call is unexpected
assertUnexpectedCall :: Has MockApi sig m => FossaApiClientF a -> m a
assertUnexpectedCall =
  send . AssertUnexpectedCall

-- | Assert that all non-repeating expectations have been satisfied
assertAllSatisfied :: Has MockApi sig m => m ()
assertAllSatisfied =
  send AssertAllSatisfied

-- | A carrier for the Mock API that holds expectations in local state
newtype MockApiC m a = MockApiC
  { runMockApiC :: StateC [ApiExpectation] m a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, Has (Lift IO) sig m) => Algebra (MockApi :+: sig) (MockApiC m) where
  alg hdl sig ctx = MockApiC $ case sig of
    L (MockApiOnce req resp) -> do
      let expectation = ApiExpectation Once req (ApiResult (Right resp))
      modify (++ [expectation])
      pure ctx
    L (MockApiAlways req resp) -> do
      let expectation = ApiExpectation Always req (ApiResult (Right resp))
      modify (++ [expectation])
      pure ctx
    L (MockApiFails req msg) -> do
      let expectation = ApiExpectation Once req (ApiResult (Left (ApiFail msg)))
      modify (++ [expectation])
      pure ctx
    L (MockApiRunExpectations req) -> do
      (<$ ctx) <$> handleRequest req
    L (AssertUnexpectedCall req) -> do
      expectations <- get
      a <-
        sendIO . assertFailure $
          "Unexpected call: \n  " <> show req <> "\n"
            <> "Unsatisfied expectations: \n  "
            <> intercalate "\n  " (map (\(ApiExpectation _ expectedReq _) -> show expectedReq) expectations)
      pure (a <$ ctx)
    L AssertAllSatisfied -> do
      remainingExpectations <- get
      let unsatisfiedSingleExpectations = filter isSingular remainingExpectations

      if null unsatisfiedSingleExpectations
        then pure ctx
        else
          sendIO . assertFailure $
            "Test completed with unsatisfied expectations: \n  "
              <> intercalate "\n  " (map (\(ApiExpectation _ req _) -> show req) unsatisfiedSingleExpectations)
    R other -> alg (runMockApiC . hdl) (R other) ctx

isSingular :: ApiExpectation -> Bool
isSingular (ApiExpectation Once _ _) = True
isSingular (ApiExpectation Always _ _) = False

-- | Matches a request to an expectation.  This function basically exists to
-- extract and compare the runtime constructor of the two arguments so that
-- arbitrary ones can be compared even if the types wouldn't match.
-- A convenient expression for building these lines is:
--   s/(\w+) ::.*/matchExpectation a@(\1{}) (ApiExpectation _ b@(\1{}) resp) = resp <$ guard (a == b)/
matchExpectation :: FossaApiClientF a -> ApiExpectation -> Maybe (ApiResult a)
matchExpectation a@(AssertRevisionBinaries{}) (ApiExpectation _ b@(AssertRevisionBinaries{}) resp) = resp <$ guard (a == b)
matchExpectation a@(AssertUserDefinedBinaries{}) (ApiExpectation _ b@(AssertUserDefinedBinaries{}) resp) = resp <$ guard (a == b)
matchExpectation a@(FinalizeLicenseScan{}) (ApiExpectation _ b@(FinalizeLicenseScan{}) resp) = resp <$ guard (a == b)
matchExpectation a@(GetApiOpts) (ApiExpectation _ b@(GetApiOpts) resp) = resp <$ guard (a == b)
matchExpectation a@(GetAttribution{}) (ApiExpectation _ b@(GetAttribution{}) resp) = resp <$ guard (a == b)
matchExpectation a@(GetIssues{}) (ApiExpectation _ b@(GetIssues{}) resp) = resp <$ guard (a == b)
matchExpectation a@(GetLatestBuild{}) (ApiExpectation _ b@(GetLatestBuild{}) resp) = resp <$ guard (a == b)
matchExpectation a@(GetLatestScan{}) (ApiExpectation _ b@(GetLatestScan{}) resp) = resp <$ guard (a == b)
matchExpectation a@(GetOrganization{}) (ApiExpectation _ b@(GetOrganization{}) resp) = resp <$ guard (a == b)
matchExpectation a@(GetProject{}) (ApiExpectation _ b@(GetProject{}) resp) = resp <$ guard (a == b)
matchExpectation a@(GetScan{}) (ApiExpectation _ b@(GetScan{}) resp) = resp <$ guard (a == b)
matchExpectation a@(GetRevisionInfo{}) (ApiExpectation _ b@(GetRevisionInfo{}) resp) = resp <$ guard (a == b)
matchExpectation a@(GetSignedLicenseScanUrl{}) (ApiExpectation _ b@(GetSignedLicenseScanUrl{}) resp) = resp <$ guard (a == b)
matchExpectation a@(ResolveProjectDependencies{}) (ApiExpectation _ b@(ResolveProjectDependencies{}) resp) = resp <$ guard (a == b)
matchExpectation a@(ResolveUserDefinedBinary{}) (ApiExpectation _ b@(ResolveUserDefinedBinary{}) resp) = resp <$ guard (a == b)
matchExpectation a@(UploadAnalysis{}) (ApiExpectation _ b@(UploadAnalysis{}) resp) = resp <$ guard (a == b)
matchExpectation a@(UploadArchive{}) (ApiExpectation _ b@(UploadArchive{}) resp) = resp <$ guard (a == b)
matchExpectation a@(UploadContainerScan{}) (ApiExpectation _ b@(UploadContainerScan{}) resp) = resp <$ guard (a == b)
matchExpectation a@(UploadContributors{}) (ApiExpectation _ b@(UploadContributors{}) resp) = resp <$ guard (a == b)
matchExpectation a@(UploadLicenseScanResult{}) (ApiExpectation _ b@(UploadLicenseScanResult{}) resp) = resp <$ guard (a == b)
matchExpectation _ _ = Nothing

-- | Handles a request in the context of the mock API.
handleRequest ::
  ( Has (State [ApiExpectation]) sig m
  ) =>
  forall a.
  FossaApiClientF a ->
  m (Maybe (ApiResult a))
handleRequest req = do
  expectations <- get
  case testExpectations req expectations of
    Just (resp, expectations') -> do
      put expectations'
      pure (Just resp)
    Nothing ->
      pure Nothing

-- | Tests a request against a list of expectations and returns the result and
-- remaining expectations.
testExpectations :: FossaApiClientF a -> [ApiExpectation] -> Maybe (ApiResult a, [ApiExpectation])
testExpectations _ [] = Nothing
testExpectations req (expectation : rest) =
  case matchExpectation req expectation of
    Nothing -> fmap (expectation :) <$> testExpectations req rest
    Just resp ->
      if isSingular expectation
        then Just (resp, rest)
        else Just (resp, expectation : rest)

type FossaApiClientMockC = SimpleC FossaApiClientF

-- | Run an action with a mock API client that tracks call expectations
runApiWithMock ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has MockApi sig m
  ) =>
  FossaApiClientMockC m a ->
  m a
runApiWithMock f = do
  result <- interpret runRequest f
  assertAllSatisfied
  pure result
  where
    runRequest ::
      ( Has Diagnostics sig m
      , Has MockApi sig m
      ) =>
      FossaApiClientF a ->
      m a
    runRequest req = do
      apiResult <- runExpectations req
      case apiResult of
        Just (ApiResult result) -> either (fatalText . unApiFail) pure result
        Nothing ->
          assertUnexpectedCall req

runMockApi ::
  ( Has (Lift IO) sig m
  ) =>
  MockApiC m a ->
  m a
runMockApi =
  evalState [] . runMockApiC
