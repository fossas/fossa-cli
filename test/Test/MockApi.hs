{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.MockApi (
  ApiExpectation,
  MockApi (..),
  MockApiC (runMockApiC),
  alwaysReturns,
  assertAllSatisfied,
  fails,
  returnsOnce,
  runMockApi,
  runApiWithMock,
) where

import Control.Algebra
import Control.Carrier.Simple (SimpleC, interpret, interpretState)
import Control.Carrier.State.Strict (StateC (StateC), runState)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.State (State, get, modify, put)
import Control.Monad (guard)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Text (Text)
import Test.HUnit (assertFailure)
import Control.Monad.Trans

data MockApi (m :: Type -> Type) a where
  MockApiOnce :: FossaApiClientF a -> a -> MockApi m ()
  MockApiAlways :: FossaApiClientF a -> a -> MockApi m ()
  MockApiFails :: FossaApiClientF a -> Text -> MockApi m ()
  MockApiRunExpectations :: FossaApiClientF a -> MockApi m (Maybe (ApiResult a))
  AssertUnexpectedCall :: FossaApiClientF a -> MockApi m a
  AssertAllSatisfied :: MockApi m ()

-- | Specifies how to handle repetition of a request
data ExpectationRepetition = Once | Always deriving (Eq, Ord, Show)

data ApiResult a = Return a | Die Text deriving (Eq, Ord, Show)

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

runExpectations :: Has MockApi sig m => FossaApiClientF a -> m (Maybe (ApiResult a))
runExpectations =
  send . MockApiRunExpectations

assertUnexpectedCall :: Has MockApi sig m => FossaApiClientF a -> m a
assertUnexpectedCall =
  send . AssertUnexpectedCall

assertAllSatisfied :: Has MockApi sig m => m ()
assertAllSatisfied =
  send AssertAllSatisfied

newtype MockApiC m a = MockApiC
  { runMockApiC :: StateC [ApiExpectation] m a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, Has (Lift IO) sig m) => Algebra (MockApi :+: sig) (MockApiC m) where
  alg hdl sig ctx = MockApiC $ case sig of
    L (MockApiOnce req resp) -> do
      let expectation = ApiExpectation Once req (Return resp)
      modify (++ [expectation])
      pure ctx
    L (MockApiAlways req resp) -> do
      let expectation = ApiExpectation Always req (Return resp)
      modify (++ [expectation])
      pure ctx
    L (MockApiFails req msg) -> do
      let expectation = ApiExpectation Once req (Die msg)
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
matchExpectation :: FossaApiClientF a -> ApiExpectation -> Maybe (ApiResult a)
matchExpectation a@(GetApiOpts) (ApiExpectation _ b@(GetApiOpts) resp) = resp <$ guard (a == b)
matchExpectation a@(GetIssues{}) (ApiExpectation _ b@(GetIssues{}) resp) = resp <$ guard (a == b)
matchExpectation a@(GetLatestBuild{}) (ApiExpectation _ b@(GetLatestBuild{}) resp) = resp <$ guard (a == b)
matchExpectation a@(GetLatestScan{}) (ApiExpectation _ b@(GetLatestScan{}) resp) = resp <$ guard (a == b)
matchExpectation a@(GetOrganization) (ApiExpectation _ b@(GetOrganization) resp) = resp <$ guard (a == b)
matchExpectation a@(GetProject{}) (ApiExpectation _ b@(GetProject{}) resp) = resp <$ guard (a == b)
matchExpectation a@(GetScan{}) (ApiExpectation _ b@(GetScan{}) resp) = resp <$ guard (a == b)
matchExpectation a@(UploadAnalysis{}) (ApiExpectation _ b@(UploadAnalysis{}) resp) = resp <$ guard (a == b)
matchExpectation a@(UploadContributors{}) (ApiExpectation _ b@(UploadContributors{}) resp) = resp <$ guard (a == b)
matchExpectation _ _ = Nothing

-- | Handles a request in the context of the mock API.
handleRequest ::
  ( Has (State [ApiExpectation]) sig m
  ) =>
  FossaApiClientF a ->
  m (Maybe (ApiResult a))
handleRequest req = do
  expectations <- get
  case testExpectations expectations of
    Just (resp, expectations') -> do
      -- sendIO . putStrLn $ "Matched expectation: " <> show req
      -- sendIO . putStrLn $ "Remaining expectations:\n  "
      --     <> intercalate "\n  " (map (\(ApiExpectation freq req _) -> show freq <> " - " <> show req) expectations')
      put expectations'
      pure (Just resp)
    -- case resp of
    --   Return a -> pure a
    --   Die msg -> fatalText msg
    Nothing ->
      pure Nothing
  where
    -- sendIO . assertFailure $
    --   "Unexpected call: \n  " <> show req <> "\n"
    --     <> "Unsatisfied expectations: \n  "
    --     <> intercalate "\n  " (map (\(ApiExpectation _ expectedReq _) -> show expectedReq) expectations)

    testExpectations [] = Nothing
    testExpectations (expectation : rest) =
      case matchExpectation req expectation of
        Nothing -> fmap (expectation :) <$> testExpectations rest
        Just resp ->
          if isSingular expectation
            then Just (resp, rest)
            else Just (resp, expectation : rest)

-- | Run an action with a mock API client that tracks call expectations
runApiWithMock ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has MockApi sig m
  ) =>
  SimpleC FossaApiClientF m a ->
  m a
runApiWithMock f = do
  result <-
    interpret
      ( \req -> do
          apiResult <- runExpectations req
          case apiResult of
            Just (Return resp) -> pure resp
            Just (Die msg) -> fatalText msg
            Nothing ->
              assertUnexpectedCall req
      )
      f
  assertAllSatisfied
  pure result

runMockApi ::
  ( Has (Lift IO) sig m
  ) =>
  MockApiC m a ->
  m a
runMockApi =
  (fmap snd) . runState [] . runMockApiC