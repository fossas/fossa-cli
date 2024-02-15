module Control.Carrier.DiagnosticsSpec (spec) where

import Control.Carrier.Diagnostics (logWithExit_)
import Control.Carrier.Stack (runStack)
import Control.Carrier.Telemetry (withoutTelemetry)
import Control.Effect.Diagnostics (ToDiagnostic (..), fatal)
import Data.Text (Text)
import Effect.Exec (ExitCode (..))
import Effect.Logger (ignoreLogger, logInfo)
import Errata (Errata (..))
import Test.Hspec (Spec, describe, it, shouldThrow)

spec :: Spec
spec = describe "logWithExit_" $ do
  it "exits on success" $ do
    successAction `shouldThrow` (== ExitSuccess)
  it "exits on failure" $ do
    failureAction `shouldThrow` (== ExitFailure 1)
  where
    successAction = withoutTelemetry . runStack . ignoreLogger . logWithExit_ $ logInfo "Action succeeded!"
    failureAction = withoutTelemetry . runStack . ignoreLogger . logWithExit_ . fatal $ TestError "Action failed!"

newtype TestError = TestError Text
  deriving (Eq, Ord, Show)

instance ToDiagnostic TestError where
  renderDiagnostic (TestError e) = Errata (Just e) [] Nothing
