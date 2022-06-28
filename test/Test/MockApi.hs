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
  alwaysReturnsForAnyRequest,
  assertAllSatisfied,
  fails,
  returnsOnce,
  returnsOnceForAnyRequest,
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
import Control.Monad.Trans (MonadIO)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Text (Text)
import Test.HUnit (assertFailure)

data MatchFailureType = NoMatchingRequest | UnmockedEndpoint
  deriving (Eq, Ord, Show)

data RequestResult a
  = RequestSatisfied a
  | RequestUnsatisfied
  | RequestUnmocked
  deriving (Show, Eq, Functor)

instance Semigroup (RequestResult a) where
  a@(RequestSatisfied{}) <> _ = a
  _ <> b@(RequestSatisfied{}) = b
  RequestUnsatisfied <> _ = RequestUnsatisfied
  _ <> b = b
instance Monoid (RequestResult a) where
  mempty = RequestUnmocked

data MockApi (m :: Type -> Type) a where
  MockApiOnce :: FossaApiClientF a -> a -> MockApi m ()
  MockApiAlways :: FossaApiClientF a -> a -> MockApi m ()
  MockApiOnceForAnyRequest :: FossaApiClientF a -> a -> MockApi m ()
  MockApiAlwaysForAnyRequest :: FossaApiClientF a -> a -> MockApi m ()
  MockApiFails :: FossaApiClientF a -> Text -> MockApi m ()
  MockApiRunExpectations :: FossaApiClientF a -> MockApi m (RequestResult (ApiResult a))
  AssertUnexpectedCall :: FossaApiClientF a -> MatchFailureType -> MockApi m a
  AssertAllSatisfied :: MockApi m ()

-- | Specifies how to handle repetition of a request
data ExpectationRepetition
  = Once
  | Always
  deriving (Eq, Ord, Show)

data ExpectationRequestType
  = ExpectingExactRequest
  | ExpectingAnyRequest
  deriving (Eq, Ord, Show)

-- | The result of a call can be either a value or a diagnostic failure
newtype ApiResult a = ApiResult (Either ApiFail a)

newtype ApiFail = ApiFail {unApiFail :: Text}

-- | An expectation of an API call made up of the request and response.
data ApiExpectation where
  ApiExpectation :: ExpectationRepetition -> ExpectationRequestType -> FossaApiClientF a -> ApiResult a -> ApiExpectation

-- | Create an expectation that will only be satisfied once.
returnsOnce :: Has MockApi sig m => FossaApiClientF a -> a -> m ()
returnsOnce req resp =
  -- ApiExpectation Once req (Return resp)
  send $ MockApiOnce req resp

-- | Create an expectation that will only be satisfied once where we do not check the value of the req
returnsOnceForAnyRequest :: Has MockApi sig m => FossaApiClientF a -> a -> m ()
returnsOnceForAnyRequest req resp =
  send $ MockApiOnceForAnyRequest req resp

-- | Create an expectation that can be satisfied multiple times.
alwaysReturns :: Has MockApi sig m => FossaApiClientF a -> a -> m ()
alwaysReturns req resp =
  -- ApiExpectation Once req (Return resp)
  send $ MockApiAlways req resp

-- | Create an expectation that can be satisfied multiple times and does not check the value of the request
alwaysReturnsForAnyRequest :: Has MockApi sig m => FossaApiClientF a -> a -> m ()
alwaysReturnsForAnyRequest req resp =
  -- ApiExpectation Once req (Return resp)
  send $ MockApiAlwaysForAnyRequest req resp

-- | Fail with a fatal diagnostic error
fails :: Has MockApi sig m => FossaApiClientF a -> Text -> m ()
fails req msg =
  -- ApiExpectation Once req (Die msg)
  send $ MockApiFails req msg

-- | Run a request against the expectations
runExpectations :: Has MockApi sig m => FossaApiClientF a -> m (RequestResult (ApiResult a))
runExpectations =
  send . MockApiRunExpectations

-- | Assert that this call is unexpected
assertUnexpectedCall :: Has MockApi sig m => FossaApiClientF a -> MatchFailureType -> m a
assertUnexpectedCall err =
  send . AssertUnexpectedCall err

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
      let expectation = ApiExpectation Once ExpectingExactRequest req (ApiResult (Right resp))
      modify (++ [expectation])
      pure ctx
    L (MockApiOnceForAnyRequest req resp) -> do
      let expectation = ApiExpectation Once ExpectingAnyRequest req (ApiResult (Right resp))
      modify (++ [expectation])
      pure ctx
    L (MockApiAlways req resp) -> do
      let expectation = ApiExpectation Always ExpectingExactRequest req (ApiResult (Right resp))
      modify (++ [expectation])
      pure ctx
    L (MockApiAlwaysForAnyRequest req resp) -> do
      let expectation = ApiExpectation Always ExpectingAnyRequest req (ApiResult (Right resp))
      modify (++ [expectation])
      pure ctx
    L (MockApiFails req msg) -> do
      let expectation = ApiExpectation Once ExpectingExactRequest req (ApiResult (Left (ApiFail msg)))
      modify (++ [expectation])
      pure ctx
    L (MockApiRunExpectations req) -> do
      (<$ ctx) <$> handleRequest req
    L (AssertUnexpectedCall req err) -> do
      expectations <- get
      a <-
        sendIO . assertFailure $
          "Unexpected call: \n  " <> show err <> ": " <> show req <> "\n"
            <> "Unsatisfied expectations: \n  "
            <> intercalate "\n  " (map (\(ApiExpectation _ _ expectedReq _) -> show expectedReq) expectations)
      pure (a <$ ctx)
    L AssertAllSatisfied -> do
      remainingExpectations <- get
      let unsatisfiedSingleExpectations = filter isSingular remainingExpectations

      if null unsatisfiedSingleExpectations
        then pure ctx
        else
          sendIO . assertFailure $
            "Test completed with unsatisfied expectations: \n  "
              <> intercalate "\n  " (map (\(ApiExpectation _ _ req _) -> show req) unsatisfiedSingleExpectations)
    R other -> alg (runMockApiC . hdl) (R other) ctx

isSingular :: ApiExpectation -> Bool
isSingular (ApiExpectation Once _ _ _) = True
isSingular (ApiExpectation Always _ _ _) = False

-- | A helper function used to check both ExpectationRequestType possibilities in matchExpectation
checkResult :: ExpectationRequestType -> FossaApiClientF a -> FossaApiClientF a -> ApiResult a -> RequestResult (ApiResult a)
checkResult ExpectingExactRequest a b resp
  | a == b = RequestSatisfied resp
  | otherwise = RequestUnsatisfied
checkResult ExpectingAnyRequest _ _ resp = RequestSatisfied resp

-- | Matches a request to an expectation.  This function basically exists to
-- extract and compare the runtime constructor of the two arguments so that
-- arbitrary ones can be compared even if the types wouldn't match.
-- A convenient expression for building these lines is:
--   s/(\w+) ::.*/matchExpectation a@(\1{}) (ApiExpectation _ requestExpectation b@(\1{}) resp) = checkResult requestExpectation a b resp/
matchExpectation :: FossaApiClientF a -> ApiExpectation -> RequestResult (ApiResult a)
matchExpectation a@(AssertRevisionBinaries{}) (ApiExpectation _ requestExpectation b@(AssertRevisionBinaries{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(AssertRevisionBinaries{}) (ApiExpectation{}) = RequestUnsatisfied
matchExpectation a@(AssertUserDefinedBinaries{}) (ApiExpectation _ requestExpectation b@(AssertUserDefinedBinaries{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(AssertUserDefinedBinaries{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(FinalizeLicenseScan{}) (ApiExpectation _ requestExpectation b@(FinalizeLicenseScan{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(FinalizeLicenseScan{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(GetApiOpts) (ApiExpectation _ requestExpectation b@(GetApiOpts) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(GetApiOpts{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(GetAttribution{}) (ApiExpectation _ requestExpectation b@(GetAttribution{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(GetAttribution{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(GetIssues{}) (ApiExpectation _ requestExpectation b@(GetIssues{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(GetIssues{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(GetLatestBuild{}) (ApiExpectation _ requestExpectation b@(GetLatestBuild{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(GetLatestBuild{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(GetLatestScan{}) (ApiExpectation _ requestExpectation b@(GetLatestScan{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(GetLatestScan{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(GetOrganization{}) (ApiExpectation _ requestExpectation b@(GetOrganization{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(GetOrganization{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(GetProject{}) (ApiExpectation _ requestExpectation b@(GetProject{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(GetProject{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(GetScan{}) (ApiExpectation _ requestExpectation b@(GetScan{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(GetScan{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(GetSignedLicenseScanUrl{}) (ApiExpectation _ requestExpectation b@(GetSignedLicenseScanUrl{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(GetSignedLicenseScanUrl{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(GetSignedUploadUrl{}) (ApiExpectation _ requestExpectation b@(GetSignedUploadUrl{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(GetSignedUploadUrl{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(QueueArchiveBuild{}) (ApiExpectation _ requestExpectation b@(QueueArchiveBuild{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(QueueArchiveBuild{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(ResolveProjectDependencies{}) (ApiExpectation _ requestExpectation b@(ResolveProjectDependencies{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(ResolveProjectDependencies{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(ResolveUserDefinedBinary{}) (ApiExpectation _ requestExpectation b@(ResolveUserDefinedBinary{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(ResolveUserDefinedBinary{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(UploadAnalysis{}) (ApiExpectation _ requestExpectation b@(UploadAnalysis{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(UploadAnalysis{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(UploadArchive{}) (ApiExpectation _ requestExpectation b@(UploadArchive{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(UploadArchive{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(UploadContainerScan{}) (ApiExpectation _ requestExpectation b@(UploadContainerScan{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(UploadContainerScan{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(UploadContributors{}) (ApiExpectation _ requestExpectation b@(UploadContributors{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(UploadContributors{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied
matchExpectation a@(UploadLicenseScanResult{}) (ApiExpectation _ requestExpectation b@(UploadLicenseScanResult{}) resp) = checkResult requestExpectation a b resp
matchExpectation _a@(UploadLicenseScanResult{}) (ApiExpectation _ _ _ _) = RequestUnsatisfied

-- | Handles a request in the context of the mock API.
handleRequest ::
  ( Has (State [ApiExpectation]) sig m
  ) =>
  forall a.
  FossaApiClientF a ->
  m (RequestResult (ApiResult a))
handleRequest req = do
  expectations <- get
  case testExpectations req expectations of
    RequestSatisfied (resp, expectations') -> do
      put expectations'
      pure $ RequestSatisfied resp
    RequestUnsatisfied ->
      pure RequestUnsatisfied
    RequestUnmocked ->
      pure RequestUnmocked

-- | Tests a request against a list of expectations and returns the result and
-- remaining expectations.
testExpectations :: FossaApiClientF a -> [ApiExpectation] -> RequestResult (ApiResult a, [ApiExpectation])
testExpectations _ [] = RequestUnmocked
testExpectations req (expectation : rest) =
  case matchExpectation req expectation of
    RequestSatisfied resp ->
      if isSingular expectation
        then RequestSatisfied (resp, rest)
        else RequestSatisfied (resp, expectation : rest)
    RequestUnsatisfied -> fmap (expectation :) <$> (RequestUnsatisfied <> testExpectations req rest)
    RequestUnmocked -> fmap (expectation :) <$> (RequestUnmocked <> testExpectations req rest)

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
        RequestSatisfied (ApiResult result) -> either (fatalText . unApiFail) pure result
        RequestUnsatisfied ->
          assertUnexpectedCall req NoMatchingRequest
        RequestUnmocked ->
          assertUnexpectedCall req UnmockedEndpoint

runMockApi ::
  ( Has (Lift IO) sig m
  ) =>
  MockApiC m a ->
  m a
runMockApi =
  evalState [] . runMockApiC
