{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Behavioural tests for "Discovery.WalkBroadcast": the broadcast fan-out
-- that drains one shared walk into a master channel and runs one consumer
-- thread per enabled strategy, each reading its own duplicate.  The four
-- cases below cover tool gating, slow-consumer isolation, broadcast
-- correctness (no starvation), and the abort/cleanup contract.
module Discovery.WalkBroadcastSpec (
  spec,
) where

import Control.Concurrent (threadDelay)
import Control.Effect.Lift (sendIO)
import Control.Exception (SomeException, fromException, throwIO, try)
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.IORef qualified as IOR
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Discovery.Filters (AllFilters (..), comboExclude, toolAllowed)
import Discovery.Walk (WalkStep (..), findFileNamed)
import Discovery.WalkBroadcast (WalkConsumer (..), withWalkBroadcast)
import Path
import Path.IO (createDir)
import System.IO.Error (IOError, isUserError, userError)
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
slowVisitor startRef parkedRef dir _subdirs _files = do
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

-- | A visitor that throws on its first listing, to model a failing consumer.
failingVisitor ::
  IOR.IORef (Set (Path Abs Dir)) ->
  Path Abs Dir ->
  [Path Abs Dir] ->
  [Path Abs File] ->
  EffectStack ((), WalkStep)
failingVisitor ref dir _subdirs _files = do
  sendIO (IOR.modifyIORef' ref (Set.insert dir))
  sendIO (throwIO (userError "consumer failing"))
  pure ((), WalkContinue)

