module Test.Effect (
  expectationFailure',
  expectFailure',
  expectFatal',
  shouldBe',
  shouldSatisfy',
  shouldStartWith',
  shouldEndWith',
  shouldContain',
  shouldMatchList',
  runTestEffects',
  it',
  fit',
  xit',
  withTempDir,
) where

import Control.Effect.Lift (Has, Lift, sendIO)
import Test.Hspec (
  Spec,
  SpecWith,
  expectationFailure,
  fit,
  it,
  runIO,
  shouldBe,
  shouldContain,
  shouldEndWith,
  shouldMatchList,
  shouldSatisfy,
  shouldStartWith,
  xit,
 )

import Control.Carrier.Diagnostics (DiagnosticsC, runDiagnostics)
import Control.Carrier.Finally (FinallyC, runFinally)
import Control.Carrier.Stack (StackC, runStack)
import Control.Carrier.StickyLogger (IgnoreStickyLoggerC, ignoreStickyLogger)
import Control.Effect.Diagnostics (Diagnostics, errorBoundary)
import Control.Effect.Finally (Finally, onExit)
import Control.Effect.FossaApiClient (FossaApiClientF)
import Data.Bits (finiteBitSize)
import Data.String.Conversion (toString)
import Diag.Result (Result (..), renderFailure)
import Effect.Exec (ExecIOC, runExecIO)
import Effect.Logger (IgnoreLoggerC, ignoreLogger, renderIt)
import Effect.ReadFS (ReadFSIOC, runReadFSIO)
import Path (Abs, Dir, Path, parseAbsDir, parseRelDir, (</>))
import Path.IO (createDirIfMissing, removeDirRecur)
import ResultUtil (expectFailure)
import System.Directory (getTemporaryDirectory)
import System.Random (randomIO)
import Test.MockApi (FossaApiClientMockC, MockApiC, runApiWithMock, runMockApi)
import Text.Printf (printf)

type EffectStack a = FinallyC (ExecIOC (ReadFSIOC (FossaApiClientMockC (DiagnosticsC (MockApiC (IgnoreLoggerC (IgnoreStickyLoggerC (StackC IO)))))))) a

-- TODO: add useful describe, naive describe' doesn't work.

it' :: String -> EffectStack () -> SpecWith ()
it' msg = it msg . runTestEffects

fit' :: String -> EffectStack () -> SpecWith ()
fit' msg = fit msg . runTestEffects

xit' :: String -> EffectStack () -> SpecWith ()
xit' msg = xit msg . runTestEffects

runTestEffects' :: EffectStack () -> Spec
runTestEffects' = runIO . runTestEffects

runTestEffects :: EffectStack () -> IO ()
runTestEffects = runStack . ignoreStickyLogger . ignoreLogger . runMockApi . handleDiag . runApiWithMock . runReadFSIO . runExecIO . runFinally
  where
    handleDiag :: (Has (Lift IO) sig m) => DiagnosticsC m () -> m ()
    handleDiag diag =
      runDiagnostics diag >>= \case
        Failure ws eg -> do
          expectationFailure' $ toString $ renderIt $ renderFailure ws eg "An issue occurred"
        Success _ _ -> pure ()

-- | Create a temporary directory with the given name.  The directory will be
-- created in the system temporary directory.  It will be deleted after use.
withTempDir :: (Has (Lift IO) sig m, Has Finally sig m) => FilePath -> (Path Abs Dir -> m a) -> m a
withTempDir name f = do
  systemTempDir <- sendIO (getTemporaryDirectory >>= parseAbsDir)
  randomJunk :: Word <- sendIO randomIO
  testRelDir <- sendIO . parseRelDir $ name ++ printf "-%.*x" (finiteBitSize randomJunk `div` 4) randomJunk
  let testDir = systemTempDir </> testRelDir
  onExit . sendIO $ removeDirRecur testDir
  sendIO $ createDirIfMissing True testDir
  f testDir

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

expectFailure' :: Has (Lift IO) sig m => Result a -> m ()
expectFailure' res = sendIO $ expectFailure res

-- | Succeeds if the action fails and fails otherwise.
expectFatal' :: (Has Diagnostics sig m, Has (Lift IO) sig m) => m a -> m ()
expectFatal' f = do
  errorBoundary f >>= expectFailure'
