module Control.TimeoutSpec (spec) where

import Control.Concurrent (newEmptyMVar, newMVar, threadDelay)
import Control.Effect.Diagnostics (
  Diagnostics,
  FailureBundle (FailureBundle),
  SomeDiagnostic (SomeDiagnostic),
  ToDiagnostic (renderDiagnostic),
  errorBoundary,
  renderFailureBundle,
 )
import Control.Effect.Lift (Has, Lift)
import Control.Timeout (
  Cancel,
  Duration (MicroSeconds),
  checkForCancel,
  durationToMicro,
  shouldCancelRightNow,
  timeout',
 )
import Control.Timeout.Internal (Cancel (Cancel))
import Data.Functor (void)
import Prettyprinter (pretty)
import Test.Effect (expectationFailure', it', shouldBe')
import Test.Hspec (Spec, describe, it, shouldBe)

tinyDuration :: Duration
tinyDuration = MicroSeconds 200

data CancelSet = CancelSet deriving (Eq, Ord, Show)

instance ToDiagnostic CancelSet where
  renderDiagnostic = pretty . show

spec :: Spec
spec = do
  describe "timeout'" $
    it "Should set the flag after the alloted time" $
      void . timeout' tinyDuration $ \token -> do
        -- Wait for 10x the duration, we should definitely have the flag set.
        -- If this test is flaky, try increasing tinyDuration
        -- timeout' makes no guarantees about when the flag will be set,
        -- other than that it will not happen before the duration is expired.
        threadDelay $ (* 10) $ durationToMicro tinyDuration
        cancelSet <- shouldCancelRightNow token
        cancelSet `shouldBe` True

  -- Normally, we can't control when Cancel tokens are set.  We expose the internals for these tests.
  describe "shouldCancelRightNow" $ do
    it "should return true when the mvar is not empty" $ do
      token <- Cancel <$> newMVar ()
      cancelSet <- shouldCancelRightNow token
      cancelSet `shouldBe` True

    it "should return false when the mvar is empty" $ do
      token <- Cancel <$> newEmptyMVar
      cancelSet <- shouldCancelRightNow token
      cancelSet `shouldBe` False

  describe "checkForCancel" $
    it' "should throw a fatal error after the alloted time" $ do
      result <- errorBoundary $ timeout' tinyDuration waitForever
      case result of
        Left (FailureBundle [] (SomeDiagnostic [] err)) -> show (renderDiagnostic err) `shouldBe'` show CancelSet
        Left bundle -> expectationFailure' $ show $ renderFailureBundle bundle
        Right () -> expectationFailure' "Expected timeoutfailure, got return value"

waitForever :: (Has Diagnostics sig m, Has (Lift IO) sig m) => Cancel -> m a
waitForever token = do
  checkForCancel CancelSet token
  waitForever token
