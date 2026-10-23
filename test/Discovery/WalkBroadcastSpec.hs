{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Behavioural tests for "Discovery.WalkBroadcast": the broadcast fan-out
-- that drains one shared walk into a master channel and runs one consumer
-- thread per enabled strategy, each reading its own duplicate.
module Discovery.WalkBroadcastSpec (
  spec,
) where

import Control.Concurrent (threadDelay)
import Control.Effect.Diagnostics (fatalText)
import Control.Effect.Lift (sendIO)
import Control.Exception (SomeException, fromException, throwIO, try)
import Control.Monad (unless)
import Data.Either (isLeft)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.IORef qualified as IOR
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Discovery.Filters (AllFilters (..), comboExclude, comboInclude, toolAllowed)
import Discovery.Walk (WalkStep (..), findFileNamed)
import Discovery.WalkBroadcast (WalkConsumer (..), accumulateConsumer, broadcastStream, withWalkBroadcast)
import Discovery.WalkStream (DirListing (..))
import Path
import Path.IO (createDir)
import Streaming (Of (..), Stream, lift)
import Streaming.Prelude qualified as S
import System.IO.Error (isUserError)
import Test.Effect
import Test.Hspec
import Types (DiscoveredProjectType (..), TargetFilter (TypeTarget))

spec :: Spec
spec =
  describe "Discovery.WalkBroadcast" $ do
    gatingSpec
    isolationSpec
    broadcastSpec
    abortSpec
    failureContractSpec

-- The tool-attributed failure message; here a stand-in.
msg :: Text
msg = "Error discovering projects for test"

-- ---------------------------------------------------------------------------
-- Gating
-- ---------------------------------------------------------------------------

gatingSpec :: Spec
gatingSpec =
  describe "tool gating" $ do
    it' "never invokes a consumer whose tool is excluded by the filters" $
      withTempDir "test-Discovery-WalkBroadcast-gate" $ \tmpDir -> do
        let web = tmpDir </> $(mkRelDir "web")
        sendIO $ do
          traverse_ createDir [web]
          writeFile (toFilePath (tmpDir </> $(mkRelFile "Cargo.toml"))) "[package]"
          writeFile (toFilePath (web </> $(mkRelFile "package.json"))) "{}"

        -- Cargo is excluded, so its consumer must not be started at all.
        let excludedFilters = (mempty :: AllFilters){excludeFilters = comboExclude [TypeTarget "cargo"] []}
        toolAllowed excludedFilters CargoProjectType `shouldBe'` False

        counterRef <- sendIO $ IOR.newIORef 0
        outRef <- sendIO $ IOR.newIORef ([] :: [Path Abs Dir])
        let consumer =
              WalkConsumer
                { wcProjectType = CargoProjectType
                , wcVisitor = countingConsumer counterRef outRef
                }
        withWalkBroadcast excludedFilters tmpDir [consumer]

        -- No invocation, no collected output: the disabled tool did no work.
        count <- sendIO $ IOR.readIORef counterRef
        count `shouldBe'` 0
        out <- sendIO $ IOR.readIORef outRef
        out `shouldSatisfy'` null

    it' "never invokes a consumer whose tool is not on the include list" $
      withTempDir "test-Discovery-WalkBroadcast-include-gate" $ \tmpDir -> do
        let web = tmpDir </> $(mkRelDir "web")
        sendIO $ do
          traverse_ createDir [web]
          writeFile (toFilePath (tmpDir </> $(mkRelFile "Cargo.toml"))) "[package]"
          writeFile (toFilePath (web </> $(mkRelFile "package.json"))) "{}"

        -- Allow-listing composer only leaves cargo outside the allowed set,
        -- so the cargo consumer must not be started at all (include-list
        -- gating, the dual of the exclude-list case above).
        let includeFilters = (mempty :: AllFilters){includeFilters = comboInclude [TypeTarget "composer"] []}
        toolAllowed includeFilters CargoProjectType `shouldBe'` False

        counterRef <- sendIO $ IOR.newIORef 0
        outRef <- sendIO $ IOR.newIORef ([] :: [Path Abs Dir])
        let consumer =
              WalkConsumer
                { wcProjectType = CargoProjectType
                , wcVisitor = countingConsumer counterRef outRef
                }
        withWalkBroadcast includeFilters tmpDir [consumer]

        -- No invocation, no collected output: the unlisted tool did no work.
        -- (The internal pins — no channel dup'd, no thread forked — are not
        -- observable from outside the module; the zero counts below are the
        -- externally visible consequence of exactly that.)
        count <- sendIO $ IOR.readIORef counterRef
        count `shouldBe'` 0
        out <- sendIO $ IOR.readIORef outRef
        out `shouldSatisfy'` null

    it' "invokes a consumer whose tool is allowed" $
      withTempDir "test-Discovery-WalkBroadcast-allow" $ \tmpDir -> do
        let web = tmpDir </> $(mkRelDir "web")
        sendIO $ do
          traverse_ createDir [web]
          writeFile (toFilePath (tmpDir </> $(mkRelFile "Cargo.toml"))) "[package]"
          writeFile (toFilePath (web </> $(mkRelFile "package.json"))) "{}"

        -- With no exclusions cargo is allowed, so the same consumer runs at
        -- every directory the walk visits.
        counterRef <- sendIO $ IOR.newIORef 0
        outRef <- sendIO $ IOR.newIORef ([] :: [Path Abs Dir])
        let consumer =
              WalkConsumer
                { wcProjectType = CargoProjectType
                , wcVisitor = countingConsumer counterRef outRef
                }
        -- Positive control: the same consumer type, allowed, is invoked twice
        -- (root + web), proving the zero count above is gating and not a
        -- no-op run.
        withWalkBroadcast (mempty :: AllFilters) tmpDir [consumer]

        count <- sendIO $ IOR.readIORef counterRef
        count `shouldSatisfy'` (> 0)
        out <- sendIO $ IOR.readIORef outRef
        length out `shouldBe'` 2

-- ---------------------------------------------------------------------------
-- Slow-consumer isolation
-- ---------------------------------------------------------------------------

isolationSpec :: Spec
isolationSpec =
  describe "slow-consumer isolation" $ do
    it' "a slow consumer does not delay another; each drains its own channel" $
      withTempDir "test-Discovery-WalkBroadcast-slow" $ \tmpDir -> do
        let web = tmpDir </> $(mkRelDir "web")
            webSrc = web </> $(mkRelDir "src")
        sendIO $ traverse_ createDir [web, webSrc]

        aStart <- sendIO $ IOR.newIORef Nothing
        parked <- sendIO $ IOR.newIORef False
        bLast <- sendIO $ IOR.newIORef Nothing
        bCollected <- sendIO $ IOR.newIORef (Set.empty :: Set (Path Abs Dir))

        -- A parks (threadDelay 200ms) on its first listing; B records the
        -- timestamp of its last listing.  Timing-sensitive but generous:
        -- B must finish its whole set within A's park window, which only
        -- fails if the broadcast is serializing consumers behind A.
        let aConsumer =
              WalkConsumer
                { wcProjectType = NpmProjectType
                , wcVisitor = slowVisitor aStart parked
                }
            bConsumer =
              WalkConsumer
                { wcProjectType = CargoProjectType
                , wcVisitor = stampedVisitor bLast bCollected
                }
        withWalkBroadcast (mempty :: AllFilters) tmpDir [aConsumer, bConsumer]

        aBegan <- sendIO $ IOR.readIORef aStart
        bDone <- sendIO $ IOR.readIORef bLast
        bDirs <- sendIO $ IOR.readIORef bCollected
        bDirs `shouldBe'` Set.fromList [tmpDir, web, webSrc]
        case (aBegan, bDone) of
          (Just s, Just l) -> l `shouldSatisfy'` (< addUTCTime 0.15 s)
          _ -> expectationFailure' "consumers did not all run"

-- ---------------------------------------------------------------------------
-- Broadcast correctness (no starvation)
-- ---------------------------------------------------------------------------

broadcastSpec :: Spec
broadcastSpec =
  describe "broadcast correctness" $ do
    it' "each consumer sees the full listing set and one consumer's WalkSkipAll does not starve another" $
      withTempDir "test-Discovery-WalkBroadcast-starve" $ \tmpDir -> do
        let web = tmpDir </> $(mkRelDir "web")
        sendIO $ do
          traverse_ createDir [web]
          writeFile (toFilePath (tmpDir </> $(mkRelFile "Cargo.toml"))) "[package]"
          writeFile (toFilePath (web </> $(mkRelFile "package.json"))) "{}"

        -- All three strategies are enabled (no filters).  Cargo-like matches
        -- at the root and skips its whole subtree; node-like matches at the
        -- nested package.json and continues; the third records everything.
        cargoRef <- sendIO $ IOR.newIORef (Set.empty :: Set (Path Abs Dir))
        nodeRef <- sendIO $ IOR.newIORef (Set.empty :: Set (Path Abs Dir))
        allRef <- sendIO $ IOR.newIORef (Set.empty :: Set (Path Abs Dir))
        let consumers =
              [ WalkConsumer{wcProjectType = CargoProjectType, wcVisitor = cargoLikeVisitor cargoRef}
              , WalkConsumer{wcProjectType = ComposerProjectType, wcVisitor = nodeLikeVisitor nodeRef}
              , WalkConsumer{wcProjectType = NpmProjectType, wcVisitor = recordingVisitor allRef}
              ]
        withWalkBroadcast (mempty :: AllFilters) tmpDir consumers

        -- The unconstrained consumer saw the full listing set.
        allDirs <- sendIO $ IOR.readIORef allRef
        allDirs `shouldSatisfy'` (== Set.fromList [tmpDir, web])
        -- Cargo-like matched at the root and swallowed its own subtree.
        cargoDirs <- sendIO $ IOR.readIORef cargoRef
        cargoDirs `shouldSatisfy'` (== Set.fromList [tmpDir])
        -- The regression: node-like still collected the nested web/ marker
        -- even though the other consumer did a WalkSkipAll at the root.
        nodeDirs <- sendIO $ IOR.readIORef nodeRef
        nodeDirs `shouldSatisfy'` (== Set.fromList [web])

-- ---------------------------------------------------------------------------
-- Abort / cleanup contract
-- ---------------------------------------------------------------------------

abortSpec :: Spec
abortSpec =
  describe "abort contract" $ do
    it' "a consumer exception propagates out of the run and every thread terminates" $
      withTempDir "test-Discovery-WalkBroadcast-abort" $ \tmpDir -> do
        let a = tmpDir </> $(mkRelDir "a")
            b = tmpDir </> $(mkRelDir "b")
        sendIO $ do
          traverse_ createDir [a, b]
          writeFile (toFilePath (a </> $(mkRelFile "a.txt"))) ""
          writeFile (toFilePath (b </> $(mkRelFile "b.txt"))) ""

        failingCollected <- sendIO $ IOR.newIORef (Set.empty :: Set (Path Abs Dir))
        okCollected <- sendIO $ IOR.newIORef (Set.empty :: Set (Path Abs Dir))
        let consumers =
              [ WalkConsumer{wcProjectType = CargoProjectType, wcVisitor = failingVisitor failingCollected}
              , WalkConsumer{wcProjectType = ComposerProjectType, wcVisitor = recordingVisitor okCollected}
              ]
        -- The failing visitor throws synchronously; the broadcast must
        -- re-throw it and kill the still-running threads, so the bracket
        -- returns with the exception rather than hanging.
        res <-
          sendIO $
            try @SomeException (runTestEffects $ withWalkBroadcast (mempty :: AllFilters) tmpDir consumers)
        -- The consumer's user error propagates out of the run (and the run
        -- returns, so every thread terminated).
        res `shouldSatisfy'` (\case Left e -> maybe False isUserError (fromException e); Right () -> False)

    it' "a walker exception surfaces and consumers are killed (the run returns)" $
      withTempDir "test-Discovery-WalkBroadcast-walkfail" $ \tmpDir -> do
        collected <- sendIO $ IOR.newIORef (Set.empty :: Set (Path Abs Dir))
        let consumer =
              WalkConsumer
                { wcProjectType = NpmProjectType
                , wcVisitor = recordingVisitor collected
                }
        -- Drive the walker's failure path through the exported
        -- 'broadcastStream' seam: a stream that yields one listing, then
        -- throws.  (Cleanest available seam — the stream parameter is part
        -- of the public API, so no test hook was needed.)
        res <-
          sendIO $
            try @SomeException
              (runTestEffects (broadcastStream (mempty :: AllFilters) (failingWalkerStream tmpDir) [consumer]))
        -- The walker's user error propagates out of the run, and the run
        -- returns rather than hanging — the consumer parked on its channel
        -- was killed by the abort path.
        res `shouldSatisfy'` (\case Left e -> maybe False isUserError (fromException e); Right () -> False)

-- ---------------------------------------------------------------------------
-- T4 failure contract: all-or-nothing per strategy, isolation between them
-- ---------------------------------------------------------------------------

failureContractSpec :: Spec
failureContractSpec =
  describe "T4 failure contract (accumulateConsumer)" $ do
    it' "a failing directory discards that strategy's earlier successful collections (all-or-nothing)" $
      withTempDir "test-Discovery-WalkBroadcast-failure-allornothing" $ \tmpDir -> do
        let sub = tmpDir </> $(mkRelDir "sub")
        sendIO $ do
          traverse_ createDir [sub]
          writeFile (toFilePath (tmpDir </> $(mkRelFile "top.txt"))) "top"
          writeFile (toFilePath (sub </> $(mkRelFile "fail-marker.txt"))) "boom"

        -- The root is collected successfully; the subdir's visitor throws.
        sink <- sendIO (IOR.newIORef (Right ([] :: [Path Abs Dir])))
        let consumer = accumulateConsumer NpmProjectType msg sink collectingFailingVisitor
        withWalkBroadcast (mempty :: AllFilters) tmpDir [consumer]

        -- The root was collected before the failure, but a failing directory
        -- fails the whole strategy: the sink holds the error, not the earlier
        -- collection (all-or-nothing).
        state <- sendIO (IOR.readIORef sink)
        state `shouldSatisfy'` isLeft

    it' "one strategy's failure leaves another strategy's collection unaffected (isolation)" $
      withTempDir "test-Discovery-WalkBroadcast-failure-isolation" $ \tmpDir -> do
        let sub = tmpDir </> $(mkRelDir "sub")
        sendIO $ do
          traverse_ createDir [sub]
          writeFile (toFilePath (tmpDir </> $(mkRelFile "top.txt"))) "top"
          writeFile (toFilePath (sub </> $(mkRelFile "fail-marker.txt"))) "boom"

        aSink <- sendIO (IOR.newIORef (Right ([] :: [Path Abs Dir])))
        bSink <- sendIO (IOR.newIORef (Right ([] :: [Path Abs Dir])))
        let consumerA = accumulateConsumer CargoProjectType msg aSink collectingFailingVisitor
            consumerB = accumulateConsumer ComposerProjectType msg bSink collectingSucceedingVisitor
        -- A fails at the subdir; B never fails.  The broadcast keeps going so B
        -- collects the whole tree while A's sink records the failure.
        withWalkBroadcast (mempty :: AllFilters) tmpDir [consumerA, consumerB]

        aState <- sendIO (IOR.readIORef aSink)
        bState <- sendIO (IOR.readIORef bSink)
        aState `shouldSatisfy'` isLeft
        bState `shouldSatisfy'` \case
          Right projects -> Set.fromList projects == Set.fromList [tmpDir, sub]
          Left _ -> False

-- ---------------------------------------------------------------------------
-- Visitors
-- ---------------------------------------------------------------------------

-- | Records every visited directory and always continues.
recordingVisitor ::
  IOR.IORef (Set (Path Abs Dir)) ->
  Path Abs Dir ->
  [Path Abs Dir] ->
  [Path Abs File] ->
  EffectStack ((), WalkStep)
recordingVisitor ref dir _subdirs _files = sendIO (IOR.modifyIORef' ref (Set.insert dir)) $> ((), WalkContinue)

-- | Cargo-like: record the directory, then 'WalkSkipAll' when it holds
-- @Cargo.toml@ (swallowing the rest of that visitor's subtree).
cargoLikeVisitor ::
  IOR.IORef (Set (Path Abs Dir)) ->
  Path Abs Dir ->
  [Path Abs Dir] ->
  [Path Abs File] ->
  EffectStack ((), WalkStep)
cargoLikeVisitor ref dir _subdirs files = do
  sendIO (IOR.modifyIORef' ref (Set.insert dir))
  pure
    ( ()
    , case findFileNamed "Cargo.toml" files of
        Just _ -> WalkSkipAll
        Nothing -> WalkContinue
    )

-- | Node-like: record the directory only where it holds @package.json@, and
-- always continue so it keeps walking past other visitors' skips.
nodeLikeVisitor ::
  IOR.IORef (Set (Path Abs Dir)) ->
  Path Abs Dir ->
  [Path Abs Dir] ->
  [Path Abs File] ->
  EffectStack ((), WalkStep)
nodeLikeVisitor ref dir _subdirs files =
  case findFileNamed "package.json" files of
    Just _ -> sendIO (IOR.modifyIORef' ref (Set.insert dir)) $> ((), WalkContinue)
    Nothing -> pure ((), WalkContinue)

-- | A visitor that records every invocation and its output, used for gating.
countingConsumer ::
  IOR.IORef Int ->
  IOR.IORef [Path Abs Dir] ->
  Path Abs Dir ->
  [Path Abs Dir] ->
  [Path Abs File] ->
  EffectStack ((), WalkStep)
countingConsumer counterRef outRef dir _subdirs _files = do
  sendIO (IOR.modifyIORef' counterRef (+ 1))
  sendIO (IOR.modifyIORef' outRef (dir :))
  pure ((), WalkContinue)

-- | A visitor that parks (200ms) on its first invocation, to model a slow
-- consumer whose stall must not block its siblings.
slowVisitor ::
  IOR.IORef (Maybe UTCTime) ->
  IOR.IORef Bool ->
  Path Abs Dir ->
  [Path Abs Dir] ->
  [Path Abs File] ->
  EffectStack ((), WalkStep)
slowVisitor startRef parkedRef _dir _subdirs _files = do
  parked <- sendIO (IOR.readIORef parkedRef)
  unless parked $ do
    now <- sendIO getCurrentTime
    sendIO $ IOR.writeIORef startRef (Just now)
    sendIO (threadDelay 200000)
    sendIO $ IOR.writeIORef parkedRef True
  pure ((), WalkContinue)

-- | Records every visited directory and the timestamp of its latest visit.
stampedVisitor ::
  IOR.IORef (Maybe UTCTime) ->
  IOR.IORef (Set (Path Abs Dir)) ->
  Path Abs Dir ->
  [Path Abs Dir] ->
  [Path Abs File] ->
  EffectStack ((), WalkStep)
stampedVisitor stampRef ref dir _subdirs _files = do
  now <- sendIO getCurrentTime
  sendIO $ IOR.writeIORef stampRef (Just now)
  sendIO (IOR.modifyIORef' ref (Set.insert dir))
  pure ((), WalkContinue)

-- | A stream that yields one listing and then throws, to model a failing
-- walker.
failingWalkerStream :: Path Abs Dir -> Stream (Of DirListing) EffectStack ()
failingWalkerStream dir = do
  S.yield (DirListing dir [] [])
  lift (sendIO (throwIO (userError "walker failing")))

-- | A visitor that throws on its first listing, to model a failing consumer.
failingVisitor ::
  IOR.IORef (Set (Path Abs Dir)) ->
  Path Abs Dir ->
  [Path Abs Dir] ->
  [Path Abs File] ->
  EffectStack ((), WalkStep)
failingVisitor ref dir _subdirs _files = do
  sendIO (IOR.modifyIORef' ref (Set.insert dir))
  _ <- sendIO (throwIO (userError "consumer failing"))
  pure ((), WalkContinue)

-- | Raw visitor that collects the directory, but throws (a per-directory
-- failure) at any directory holding @fail-marker.txt@.  Drives the all-or-
-- nothing case: the sink records the error and discards earlier collections.
collectingFailingVisitor ::
  Path Abs Dir ->
  [Path Abs Dir] ->
  [Path Abs File] ->
  EffectStack ([Path Abs Dir], WalkStep)
collectingFailingVisitor dir _subdirs files =
  if isJust (findFileNamed "fail-marker.txt" files)
    -- Throw a Diagnostics-level failure (the kind 'warnThenRecover' recovers);
    -- a raw 'throwIO' would abort the shared walk, which is the accepted delta.
    then fatalText "visitor failing"
    else pure ([dir], WalkContinue)

-- | Raw visitor that always collects the directory (never fails).  Drives the
-- isolation case: it keeps its whole collection while a sibling fails.
collectingSucceedingVisitor ::
  Path Abs Dir ->
  [Path Abs Dir] ->
  [Path Abs File] ->
  EffectStack ([Path Abs Dir], WalkStep)
collectingSucceedingVisitor dir _subdirs _files = pure ([dir], WalkContinue)
