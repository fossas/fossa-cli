module Test.Effect (
  expectationFailure',
  shouldBe',
  shouldSatisfy',
  shouldStartWith',
  shouldEndWith',
  shouldContain',
  shouldMatchList',
  it',
  runTestEffects',
) where

import Control.Effect.Lift (Has, Lift, sendIO)
import Test.Hspec (
  Spec,
  SpecWith,
  expectationFailure,
  it,
  runIO,
  shouldBe,
  shouldContain,
  shouldEndWith,
  shouldMatchList,
  shouldSatisfy,
  shouldStartWith,
 )

import Control.Carrier.Diagnostics (DiagnosticsC, renderFailureBundle, runDiagnostics)
import Data.String.Conversion (toString)
import Effect.Exec (ExecIOC, runExecIO)
import Effect.Logger (IgnoreLoggerC, ignoreLogger, renderIt)
import Effect.ReadFS (ReadFSIOC, runReadFSIO)

type EffectStack a = ExecIOC (ReadFSIOC (DiagnosticsC (IgnoreLoggerC IO))) a

-- TODO: add useful describe, naive describe' doesn't work.

it' :: String -> EffectStack () -> SpecWith ()
it' msg = it msg . runTestEffects

runTestEffects' :: EffectStack () -> Spec
runTestEffects' = runIO . runTestEffects

runTestEffects :: EffectStack () -> IO ()
runTestEffects = ignoreLogger . handleDiag . runReadFSIO . runExecIO
  where
    handleDiag :: (Has (Lift IO) sig m) => DiagnosticsC m () -> m ()
    handleDiag diag =
      runDiagnostics diag >>= \case
        Left err -> do
          expectationFailure' $ toString $ renderIt $ renderFailureBundle err
        Right _ -> pure ()

expectationFailure' :: (Has (Lift IO) sig m) => String -> m ()
expectationFailure' = sendIO . expectationFailure

shouldBe' :: (Has (Lift IO) sig m, Show a, Eq a) => a -> a -> m ()
shouldBe' left right = sendIO $ shouldBe left right

shouldSatisfy' :: (Has (Lift IO) sig m, Show a) => a -> (a -> Bool) -> m ()
shouldSatisfy' item predicate = sendIO $ shouldSatisfy item predicate

shouldStartWith' :: (Has (Lift IO) sig m, Show a, Eq a) => [a] -> [a] -> m ()
shouldStartWith' list prefix = sendIO $ shouldStartWith list prefix

shouldEndWith' :: (Has (Lift IO) sig m, Show a, Eq a) => [a] -> [a] -> m ()
shouldEndWith' list suffix = sendIO $ shouldEndWith list suffix

shouldContain' :: (Has (Lift IO) sig m, Show a, Eq a) => [a] -> [a] -> m ()
shouldContain' list sublist = sendIO $ shouldContain list sublist

shouldMatchList' :: (Has (Lift IO) sig m, Show a, Eq a) => [a] -> [a] -> m ()
shouldMatchList' a b = sendIO $ shouldMatchList a b
