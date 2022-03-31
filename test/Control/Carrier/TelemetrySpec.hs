module Control.Carrier.TelemetrySpec (spec) where

import Control.Carrier.Diagnostics (logWithExit_)
import Control.Carrier.Stack (runStack)
import Control.Carrier.Telemetry (withTelemetry)
import Control.Effect.Diagnostics (fatalText)
import Effect.Exec (ExitCode (..))
import Effect.Logger (ignoreLogger, logInfo)
import Test.Hspec (Spec, describe, it, shouldThrow)

spec :: Spec
spec = do
  describe "withTelemetry" $ do
    it "exits on success" $
      successAction `shouldThrow` (== ExitSuccess)

    it "exits on failure" $
      failureAction `shouldThrow` (== ExitFailure 1)
  where
    successAction = withTelemetry . runStack . ignoreLogger . logWithExit_ $ do
      logInfo "Action succeeded!"

    failureAction = withTelemetry . runStack . ignoreLogger . logWithExit_ $ do
      fatalText "Action failed!"
