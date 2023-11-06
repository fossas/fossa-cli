module Test.Effect (
  EffectStack,
  expectationFailure',
  expectFailure',
  expectSuccess',
  expectNonFatal',
  expectFatal',
  shouldBe',
  shouldBeSupersetOf',
  shouldSatisfy',
  shouldStartWith',
  shouldEndWith',
  shouldContain',
  shouldNotContain',
  shouldMatchList',
  runTestEffects',
  it',
  fit',
  xit',
  withTempDir,
  handleDiag,
) where

import Control.Carrier.ContainerRegistryApi (runContainerRegistryApi)
import Control.Carrier.ContainerRegistryApi.Common (RegistryCtx)
import Control.Carrier.Diagnostics (DiagnosticsC, runDiagnostics)
import Control.Carrier.Finally (FinallyC, runFinally)
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.Simple (SimpleC)
import Control.Carrier.Stack (StackC, runStack)
import Control.Carrier.StickyLogger (IgnoreStickyLoggerC, ignoreStickyLogger)
import Control.Effect.ContainerRegistryApi (ContainerRegistryApiF)
import Control.Effect.Diagnostics (Diagnostics, errorBoundary)
import Control.Effect.Finally (Finally, onExit)
import Control.Effect.Lift (Has, Lift, sendIO)
import Data.Bits (finiteBitSize)
import Data.Set (fromList, isSubsetOf)
import Data.String.Conversion (toString)
import Diag.Result (Result (..), renderFailure)
import Discovery.Filters (AllFilters)
import Effect.Exec (ExecIOC, runExecIO)
import Effect.Logger (IgnoreLoggerC, ignoreLogger, renderIt)
import Effect.ReadFS (ReadFSIOC, runReadFSIO)
import Path (Abs, Dir, Path, parseAbsDir, parseRelDir, (</>))
import Path.IO (createDirIfMissing, removeDirRecur)
import ResultUtil (expectFailure, assertOnSuccess, expectSuccess)
import System.Directory (getTemporaryDirectory)
import System.Random (randomIO)
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
  shouldNotContain,
  shouldSatisfy,
  shouldStartWith,
  xit,
 )
import Test.MockApi (FossaApiClientMockC, MockApiC, runApiWithMock, runMockApi)
import Text.Printf (printf)
import Type.Operator (type ($))

-- The FossaApiClient must be outside of Diagnostics because it
-- depends on it.
-- MockApiC must be inside Diagnostics so its state is not
-- lost when diagnostics short-circuits on an error.
type EffectStack =
  FinallyC
    $ SimpleC ContainerRegistryApiF
    $ ReaderC RegistryCtx
    $ ReaderC AllFilters
    $ ExecIOC
    $ ReadFSIOC
    $ FossaApiClientMockC
    $ DiagnosticsC
    $ MockApiC
    $ IgnoreLoggerC
    $ IgnoreStickyLoggerC
    $ StackC IO

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
runTestEffects = runStack . ignoreStickyLogger . ignoreLogger . runMockApi . handleDiag . runApiWithMock . runReadFSIO . runExecIO . runReader mempty . runContainerRegistryApi . runFinally

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

shouldNotContain' :: (Has (Lift IO) sig m, Show a, Eq a) => [a] -> [a] -> m ()
shouldNotContain' list sublist = sendIO $ shouldNotContain list sublist

shouldMatchList' :: (Has (Lift IO) sig m, Show a, Eq a) => [a] -> [a] -> m ()
shouldMatchList' a b = sendIO $ shouldMatchList a b

shouldBeSupersetOf' :: (Has (Lift IO) sig m, Show a, Ord a) => [a] -> [a] -> m ()
shouldBeSupersetOf' list sublist = sendIO $ shouldSatisfy (fromList list) $ isSubsetOf (fromList sublist)

expectFailure' :: Has (Lift IO) sig m => Result a -> m ()
expectFailure' res = sendIO $ expectFailure res

expectSuccess' :: Has (Lift IO) sig m => Result a -> m ()
expectSuccess' res = sendIO $ expectSuccess res

-- | Succeeds if the action fails and fails otherwise.
expectFatal' :: (Has Diagnostics sig m, Has (Lift IO) sig m) => m a -> m ()
expectFatal' f = do
  errorBoundary f >>= expectFailure'

expectNonFatal' ::  (Has Diagnostics sig m, Has (Lift IO) sig m) => m a -> m ()
expectNonFatal' f = errorBoundary f >>= expectSuccess'
  
