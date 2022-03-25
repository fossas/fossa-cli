{-# LANGUAGE GADTs #-}

module Test.MockApi (ApiExpectation, returnsOnce, alwaysReturns, runApi) where

import Control.Algebra (Has)
import Control.Carrier.FossaApiClient (FossaApiClientC)
import Control.Carrier.Simple (SimpleC, interpretState)
import Control.Carrier.State.Strict (StateC)
import Control.Effect.FossaApiClient (FossaApiClientF (..))
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.State (State, get, put)
import Control.Monad (guard)
import Data.List (intercalate)
import Test.HUnit (assertFailure)

data ExpectationRepetition = Once | Always deriving (Eq, Ord, Show)

data ApiExpectation where
  ApiExpectation :: ExpectationRepetition -> FossaApiClientF a -> a -> ApiExpectation

returnsOnce :: FossaApiClientF a -> a -> ApiExpectation
returnsOnce = ApiExpectation Once

alwaysReturns :: FossaApiClientF a -> a -> ApiExpectation
alwaysReturns = ApiExpectation Always

isSingular :: ApiExpectation -> Bool
isSingular (ApiExpectation Once _ _) = True
isSingular (ApiExpectation Always _ _) = False

matchExpectation :: FossaApiClientF a -> ApiExpectation -> Maybe a
matchExpectation a@(GetIssues _) (ApiExpectation _ b@(GetIssues _) resp) = resp <$ guard (a == b)
matchExpectation a@(GetLatestBuild _) (ApiExpectation _ b@(GetLatestBuild _) resp) = resp <$ guard (a == b)
matchExpectation a@(GetLatestScan _ _) (ApiExpectation _ b@(GetLatestScan _ _) resp) = resp <$ guard (a == b)
matchExpectation a@(GetOrganization) (ApiExpectation _ b@(GetOrganization) resp) = resp <$ guard (a == b)
matchExpectation a@(GetProject _) (ApiExpectation _ b@(GetProject _) resp) = resp <$ guard (a == b)
matchExpectation a@(GetScan _ _) (ApiExpectation _ b@(GetScan _ _) resp) = resp <$ guard (a == b)
matchExpectation _ _ = Nothing

expectedApi :: (Has (Lift IO) sig m, Has (State [ApiExpectation]) sig m) => FossaApiClientF a -> m a
expectedApi req = do
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
          <> intercalate "\n  " (map (\(ApiExpectation _ req _) -> show req) expectations)
  where
    testExpectations [] = Nothing
    testExpectations (expectation : rest) =
      case matchExpectation req expectation of
        Nothing -> fmap (expectation :) <$> testExpectations rest
        Just resp ->
          if isSingular expectation
            then Just (resp, rest)
            else Just (resp, expectation : rest)

runApi ::
  ( Has (Lift IO) sig m
  ) =>
  [ApiExpectation] ->
  SimpleC FossaApiClientF (StateC [ApiExpectation] m) a ->
  m a
runApi expectations f = do
  (remainingExpectations, result) <- interpretState expectations expectedApi f
  let unsatisfiedSingleExpectations = filter isSingular remainingExpectations
  if null unsatisfiedSingleExpectations
    then pure result
    else
      sendIO . assertFailure $
        "Unsatisfied expectations: \n  "
          <> intercalate "\n  " (map (\(ApiExpectation _ req _) -> show req) unsatisfiedSingleExpectations)
