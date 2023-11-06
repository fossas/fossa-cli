module ResultUtil (
  expectFailure,
  expectSuccess,
  assertOnSuccess,
) where

import Diag.Result (EmittedWarn, Result (Failure, Success), renderFailure)
import Test.Hspec (Expectation, expectationFailure)

expectFailure :: Result a -> Expectation
expectFailure (Failure _ _) = pure ()
expectFailure (Success _ _) = expectationFailure "expected a failure"

expectSuccess :: Result a -> Expectation
expectSuccess (Failure _ _) = expectationFailure "expected success"
expectSuccess (Success _ _) = pure ()


assertOnSuccess :: Result a -> ([EmittedWarn] -> a -> Expectation) -> Expectation
assertOnSuccess (Failure ws eg) _ = expectationFailure (show (renderFailure ws eg "An issue occurred"))
assertOnSuccess (Success ws a) f = f ws a
