{-# LANGUAGE GADTs #-}

module Test.MockApi (ApiExpectation, returnsOnce, alwaysReturns, runWithApiExpectations) where

import Control.Algebra (Has)
import Control.Carrier.Simple (SimpleC, interpretState)
import Control.Carrier.State.Strict (StateC)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.State (State, get, put)
import Control.Monad (guard)
import Data.List (intercalate)
import Test.HUnit (assertFailure)

-- | Specifies how to handle repetition of a request
data ExpectationRepetition = Once | Always deriving (Eq, Ord, Show)

-- | An expectation of an API call made up of the request and response.
data ApiExpectation where
  ApiExpectation :: ExpectationRepetition -> FossaApiClientF a -> a -> ApiExpectation

-- | Create an expectation that will only be satisfied once.
returnsOnce :: FossaApiClientF a -> a -> ApiExpectation
returnsOnce = ApiExpectation Once

-- | Create an expectation that can be satisfied multiple times.
alwaysReturns :: FossaApiClientF a -> a -> ApiExpectation
alwaysReturns = ApiExpectation Always

isSingular :: ApiExpectation -> Bool
isSingular (ApiExpectation Once _ _) = True
isSingular (ApiExpectation Always _ _) = False

-- | Matches a request to an expectation.  This function basically exists to
-- extract and compare the runtime constructor of the two arguments so that
-- arbitrary ones can be compared even if the types wouldn't match.
matchExpectation :: FossaApiClientF a -> ApiExpectation -> Maybe a
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
handleRequest :: (Has (Lift IO) sig m, Has (State [ApiExpectation]) sig m) => FossaApiClientF a -> m a
handleRequest req = do
  expectations <- get
  case testExpectations expectations of
    Just (resp, expectations') -> do
      -- sendIO . putStrLn $ "Matched expectation: " <> show req
      -- sendIO . putStrLn $ "Remaining expectations:\n  "
      --     <> intercalate "\n  " (map (\(ApiExpectation freq req _) -> show freq <> " - " <> show req) expectations')
      put expectations'
      pure resp
    Nothing ->
      sendIO . assertFailure $
        "Unexpected call: \n  " <> show req <> "\n"
          <> "Unsatisfied expectations: \n  "
          <> intercalate "\n  " (map (\(ApiExpectation _ expectedReq _) -> show expectedReq) expectations)
  where
    testExpectations [] = Nothing
    testExpectations (expectation : rest) =
      case matchExpectation req expectation of
        Nothing -> fmap (expectation :) <$> testExpectations rest
        Just resp ->
          if isSingular expectation
            then Just (resp, rest)
            else Just (resp, expectation : rest)

-- | Run an action with a mock API client that tracks call expectations
runWithApiExpectations ::
  ( Has (Lift IO) sig m
  ) =>
  [ApiExpectation] ->
  SimpleC FossaApiClientF (StateC [ApiExpectation] m) a ->
  m a
runWithApiExpectations expectations f = do
  (remainingExpectations, result) <- interpretState expectations handleRequest f
  let unsatisfiedSingleExpectations = filter isSingular remainingExpectations
  if null unsatisfiedSingleExpectations
    then pure result
    else
      sendIO . assertFailure $
        "Unsatisfied expectations: \n  "
          <> intercalate "\n  " (map (\(ApiExpectation _ req _) -> show req) unsatisfiedSingleExpectations)
