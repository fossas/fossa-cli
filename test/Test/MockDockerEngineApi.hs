{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.MockDockerEngineApi (
  ApiExpectation,
  DockerEngineApiMockC,
  MockApi (..),
  MockApiC (runMockApiC),
  alwaysReturns,
  assertAllSatisfied,
  fails,
  runMockApi,
  runApiWithMock,
) where

import Control.Algebra (Algebra (..), Has, send, type (:+:) (..))
import Control.Carrier.Simple (SimpleC, interpret)
import Control.Carrier.State.Strict (StateC (StateC), evalState)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.DockerEngineApi (DockerEngineApiF (ExportImage, GetImageSize, IsDockerEngineAccessible))
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.State (State, get, modify, put)
import Control.Monad (guard)
import Control.Monad.Trans (MonadIO)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Text (Text)
import Test.HUnit (assertFailure)

type DockerEngineApiMockC = SimpleC DockerEngineApiF

data MockApi (m :: Type -> Type) a where
  MockApiAlways :: DockerEngineApiF a -> a -> MockApi m ()
  MockApiFails :: DockerEngineApiF a -> Text -> MockApi m ()
  MockApiRunExpectations :: DockerEngineApiF a -> MockApi m (Maybe (ApiResult a))
  AssertUnexpectedCall :: DockerEngineApiF a -> MockApi m a
  AssertAllSatisfied :: MockApi m ()

data ExpectationRepetition
  = Once
  | Always
  deriving (Eq, Ord, Show)

data ExpectationRequestType
  = ExpectingExactRequest
  | ExpectingAnyRequest
  deriving (Eq, Ord, Show)

newtype ApiResult a = ApiResult (Either ApiFail a)
newtype ApiFail = ApiFail {unApiFail :: Text}

-- | An expectation of an API call made up of the request and response.
data ApiExpectation where
  ApiExpectation :: ExpectationRepetition -> ExpectationRequestType -> DockerEngineApiF a -> ApiResult a -> ApiExpectation

alwaysReturns :: Has MockApi sig m => DockerEngineApiF a -> a -> m ()
alwaysReturns req resp = send $ MockApiAlways req resp

fails :: Has MockApi sig m => DockerEngineApiF a -> Text -> m ()
fails req msg = send $ MockApiFails req msg

assertAllSatisfied :: Has MockApi sig m => m ()
assertAllSatisfied = send AssertAllSatisfied

assertUnexpectedCall :: Has MockApi sig m => DockerEngineApiF a -> m a
assertUnexpectedCall = send . AssertUnexpectedCall

runExpectations :: Has MockApi sig m => DockerEngineApiF a -> m (Maybe (ApiResult a))
runExpectations = send . MockApiRunExpectations

newtype MockApiC m a = MockApiC
  { runMockApiC :: StateC [ApiExpectation] m a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, Has (Lift IO) sig m) => Algebra (MockApi :+: sig) (MockApiC m) where
  alg hdl sig ctx = MockApiC $ case sig of
    L (MockApiAlways req resp) -> do
      let expectation = ApiExpectation Always ExpectingExactRequest req (ApiResult (Right resp))
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
isSingular _ = False

checkResult :: ExpectationRequestType -> DockerEngineApiF a -> DockerEngineApiF a -> ApiResult a -> Maybe (ApiResult a)
checkResult ExpectingExactRequest a b resp = resp <$ guard (a == b)
checkResult ExpectingAnyRequest _ _ resp = pure resp

matchExpectation :: DockerEngineApiF a -> ApiExpectation -> Maybe (ApiResult a)
matchExpectation a@(ExportImage{}) (ApiExpectation _ requestExpectation b@(ExportImage{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(GetImageSize{}) (ApiExpectation _ requestExpectation b@(GetImageSize{}) resp) = checkResult requestExpectation a b resp
matchExpectation a@(IsDockerEngineAccessible{}) (ApiExpectation _ requestExpectation b@(IsDockerEngineAccessible{}) resp) = checkResult requestExpectation a b resp
matchExpectation _ _ = Nothing

handleRequest ::
  (Has (State [ApiExpectation]) sig m) =>
  forall a.
  DockerEngineApiF a ->
  m (Maybe (ApiResult a))
handleRequest req = do
  expectations <- get
  case testExpectations req expectations of
    Just (resp, expectations') -> do
      put expectations'
      pure (Just resp)
    Nothing ->
      pure Nothing

testExpectations :: DockerEngineApiF a -> [ApiExpectation] -> Maybe (ApiResult a, [ApiExpectation])
testExpectations _ [] = Nothing
testExpectations req (expectation : rest) =
  case matchExpectation req expectation of
    Nothing -> fmap (expectation :) <$> testExpectations req rest
    Just resp ->
      if isSingular expectation
        then Just (resp, rest)
        else Just (resp, expectation : rest)

runApiWithMock ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has MockApi sig m
  ) =>
  DockerEngineApiMockC m a ->
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
      DockerEngineApiF a ->
      m a
    runRequest req = do
      apiResult <- runExpectations req
      case apiResult of
        Just (ApiResult result) -> either (fatalText . unApiFail) pure result
        Nothing ->
          assertUnexpectedCall req

runMockApi ::
  (Has (Lift IO) sig m) =>
  MockApiC m a ->
  m a
runMockApi =
  evalState [] . runMockApiC
