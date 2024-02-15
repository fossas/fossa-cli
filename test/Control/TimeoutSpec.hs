module Control.TimeoutSpec (spec) where

import Control.Concurrent (newEmptyMVar, newMVar, threadDelay)
import Control.Effect.Diagnostics (
  Diagnostics,
  ToDiagnostic (renderDiagnostic),
  errorBoundary,
 )
import Control.Effect.Lift (Has, Lift)
import Control.Timeout (
  Cancel,
  Duration (..),
  checkForCancel,
  durationToMicro,
  shouldCancelRightNow,
  timeout',
 )
import Control.Timeout.Internal (Cancel (Cancel))
import Data.Functor (void)
import Data.String.Conversion (toText)
import Errata (Errata (..))
import Test.Effect (expectFailure', it')
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

-- This needs to be at least 50ms to not be flaky
shortDuration :: Duration
shortDuration = MilliSeconds 200

data CancelSet = CancelSet deriving (Eq, Ord, Show)

instance ToDiagnostic CancelSet where
  renderDiagnostic c = Errata (Just $ toText (show c)) [] Nothing

spec :: Spec
spec = do
  describe "timeout'" $
    it "Should set the flag after the alloted time" $
      void . timeout' shortDuration $ \token -> do
        -- Wait for 2x the duration, we should definitely have the flag set.
        -- If this test is flaky, try increasing shortDuration
        -- timeout' makes no guarantees about when the flag will be set,
        -- other than that it will not happen before the duration is expired.
        threadDelay $ (* 2) $ durationToMicro shortDuration
        cancelSet <- shouldCancelRightNow token
        if cancelSet
          then pure ()
          else expectationFailure "Cancel flag was not set in time. If you repeatedly see this issue, increase the shortDuration in this module."

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
      errorBoundary (timeout' shortDuration waitForever) >>= expectFailure'

waitForever :: (Has Diagnostics sig m, Has (Lift IO) sig m) => Cancel -> m a
waitForever token = do
  checkForCancel CancelSet token
  waitForever token
