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
import Control.Monad (guard)
import Control.Monad.Trans (MonadIO)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Text (Text)
import Test.HUnit (assertFailure)

data MockApi (m :: Type -> Type) a where
  MockApiOnce :: FossaApiClientF a -> a -> MockApi m ()
  MockApiAlways :: FossaApiClientF a -> a -> MockApi m ()
  MockApiOnceForAnyRequest :: FossaApiClientF a -> a -> MockApi m ()
  MockApiAlwaysForAnyRequest :: FossaApiClientF a -> a -> MockApi m ()
  MockApiFails :: FossaApiClientF a -> Text -> MockApi m ()
  MockApiRunExpectations :: FossaApiClientF a -> MockApi m (Maybe (ApiResult a))
  AssertUnexpectedCall :: FossaApiClientF a -> MockApi m a
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
    L (AssertUnexpectedCall req) -> do
      expectations <- get
      a <-
        sendIO . assertFailure $
          "Unexpected call: \n  "
            <> show req
            <> "\n"
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
checkResult :: ExpectationRequestType -> FossaApiClientF a -> FossaApiClientF a -> ApiResult a -> Maybe (ApiResult a)
checkResult ExpectingExactRequest a b resp = resp <$ guard (a == b)
checkResult ExpectingAnyRequest _ _ resp = pure resp

-- | Matches a request to an expectation.  This function basically exists to
-- extract and compare the runtime constructor of the two arguments so that
-- arbitrary ones can be compared even if the types wouldn't match.
-- A convenient expression for building these lines is:
--   s/(\w+) ::.*/matchExpectation a@(\1{}) (ApiExpectation _ requestExpectation b@(\1{}) resp) = checkResult requestExpectation a b resp/
matchExpectation :: FossaApiClientF a -> ApiExpectation -> Maybe (ApiResult a)
matchExpectation a@(AssertRevisionBinaries{}) (ApiExpectation _ requestExpectation b@(AssertRevisionBinaries{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(AssertUserDefinedBinaries{}) (ApiExpectation _ requestExpectation b@(AssertUserDefinedBinaries{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(FinalizeLicenseScan{}) (ApiExpectation _ requestExpectation b@(FinalizeLicenseScan{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetAnalyzedRevisions{}) (ApiExpectation _ requestExpectation b@(GetAnalyzedRevisions{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetApiOpts) (ApiExpectation _ requestExpectation b@(GetApiOpts) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetAttribution{}) (ApiExpectation _ requestExpectation b@(GetAttribution{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetIssues{}) (ApiExpectation _ requestExpectation b@(GetIssues{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetLatestBuild{}) (ApiExpectation _ requestExpectation b@(GetLatestBuild{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetOrganization{}) (ApiExpectation _ requestExpectation b@(GetOrganization{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetEndpointVersion{}) (ApiExpectation _ requestExpectation b@(GetEndpointVersion{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetProject{}) (ApiExpectation _ requestExpectation b@(GetProject{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetRevisionDependencyCacheStatus{}) (ApiExpectation _ requestExpectation b@(GetRevisionDependencyCacheStatus{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetSignedLicenseScanUrl{}) (ApiExpectation _ requestExpectation b@(GetSignedLicenseScanUrl{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetSignedFirstPartyScanUrl{}) (ApiExpectation _ requestExpectation b@(GetSignedFirstPartyScanUrl{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetSignedUploadUrl{}) (ApiExpectation _ requestExpectation b@(GetSignedUploadUrl{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(QueueArchiveBuild{}) (ApiExpectation _ requestExpectation b@(QueueArchiveBuild{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(ResolveProjectDependencies{}) (ApiExpectation _ requestExpectation b@(ResolveProjectDependencies{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(ResolveUserDefinedBinary{}) (ApiExpectation _ requestExpectation b@(ResolveUserDefinedBinary{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(UploadAnalysis{}) (ApiExpectation _ requestExpectation b@(UploadAnalysis{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(UploadAnalysisWithFirstPartyLicenses{}) (ApiExpectation _ requestExpectation b@(UploadAnalysisWithFirstPartyLicenses{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(UploadFirstPartyScanResult{}) (ApiExpectation _ requestExpectation b@(UploadFirstPartyScanResult{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(UploadArchive{}) (ApiExpectation _ requestExpectation b@(UploadArchive{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(UploadNativeContainerScan{}) (ApiExpectation _ requestExpectation b@(UploadNativeContainerScan{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(UploadContributors{}) (ApiExpectation _ requestExpectation b@(UploadContributors{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(UploadLicenseScanResult{}) (ApiExpectation _ requestExpectation b@(UploadLicenseScanResult{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetPathDependencyScanUrl{}) (ApiExpectation _ requestExpectation b@(GetPathDependencyScanUrl{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(FinalizeLicenseScanForPathDependency{}) (ApiExpectation _ requestExpectation b@(FinalizeLicenseScanForPathDependency{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetAnalyzedPathRevisions{}) (ApiExpectation _ requestExpectation b@(GetAnalyzedPathRevisions{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(UploadContentForReachability{}) (ApiExpectation _ requestExpectation b@(UploadContentForReachability{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(UploadBuildForReachability{}) (ApiExpectation _ requestExpectation b@(UploadBuildForReachability{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetTokenType{}) (ApiExpectation _ requestExpectation b@(GetTokenType{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetSubscription{}) (ApiExpectation _ requestExpectation b@(GetSubscription{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetCustomBuildPermissons{}) (ApiExpectation _ requestExpectation b@(GetCustomBuildPermissons{}) resp) = checkResult requestExpectation a b resp
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
