{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Discovery.WalkStreamSpec (
  spec,
) where

import Control.Carrier.Reader (runReader)
import Control.Effect.Diagnostics (Diagnostics, warnThenRecover)
import Control.Effect.Lift (Has, Lift, sendIO)
import Data.Foldable (for_, traverse_)
import Data.Functor (void, ($>))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Discovery.Filters (AllFilters)
import Discovery.Simple (cachedDiscover)
import Discovery.Walk
import Discovery.WalkStream
import Discovery.WalkVisitor (runVisitors)
import Path
import Path.IO (createDir, createDirLink)
import Strategy.Cargo qualified as Cargo
import Strategy.Composer qualified as Composer
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S
import Test.Effect
import Test.Fixtures (excludePath)
import Test.Hspec
import Types (DiscoveredProject (..), DiscoveredProjectType (CargoProjectType, ComposerProjectType))

spec :: Spec
spec =
  describe "Discovery.WalkStream" $ do
    it' "emits a listing for every directory the plain walk visits" $
      withTempDir "test-Discovery-WalkStream-basic" $ \tmpDir -> do
        let dirs@[a, ab, b, bc] =
              map
                (tmpDir </>)
                [ $(mkRelDir "a")
                , $(mkRelDir "a/b")
                , $(mkRelDir "b")
                , $(mkRelDir "b/c")
                ]
        sendIO $ do
          traverse_ createDir dirs
          writeFile (toFilePath (tmpDir </> $(mkRelFile "top.txt"))) "top"
          writeFile (toFilePath (a </> $(mkRelFile "a.txt"))) "a"
          writeFile (toFilePath (ab </> $(mkRelFile "ab.txt"))) "ab"
          writeFile (toFilePath (b </> $(mkRelFile "b.txt"))) "b"
          writeFile (toFilePath (bc </> $(mkRelFile "bc.txt"))) "bc"

        emitted <- collect (walkStreamUnfiltered tmpDir)
        reference <- collectWalk tmpDir

        normalizeSet emitted `shouldBe'` normalizeSet (map toListing reference)
        sendIO $ traverse_ (const (pure ())) [a, ab, b, bc]

    it' "prunes directories excluded by path filters" $
      withTempDir "test-Discovery-WalkStream-filter" $ \tmpDir -> do
        let dirs@[a, ab, abc, b] =
              map
                (tmpDir </>)
                [ $(mkRelDir "a")
                , $(mkRelDir "a/b")
                , $(mkRelDir "a/b/c")
                , $(mkRelDir "b")
                ]
        sendIO $ traverse_ createDir dirs

        let filters = excludePath $(mkRelDir "a/b")

        emitted <- collect (walkStream filters tmpDir)
        reference <- collectWalkFiltered filters tmpDir

        normalizeSet emitted `shouldBe'` normalizeSet (map toListing reference)
        let visited = Set.map dlDir (Set.fromList emitted)
        Set.member ab visited `shouldBe'` False
        -- The allowed ancestor 'a' and sibling 'b' must still be visited.
        Set.member a visited `shouldBe'` True
        Set.member b visited `shouldBe'` True
        sendIO $ traverse_ (const (pure ())) [ab, abc]

    it' "terminates and matches the walk under symlink loops" $
      withTempDir "test-Discovery-WalkStream-loops" $ \tmpDir -> do
        let dirs@[lib, _, _] =
              map
                (tmpDir </>)
                [ $(mkRelDir "lib")
                , $(mkRelDir "lib/pg")
                , $(mkRelDir "lib/sqlite")
                ]
        sendIO $ do
          traverse_ createDir dirs
          -- Each of these symlinks forms a loop.
          createDirLink lib (lib </> $(mkRelDir "pg/lib"))
          createDirLink lib (lib </> $(mkRelDir "sqlite/lib"))

        emitted <- collect (walkStreamUnfiltered tmpDir)
        reference <- collectWalk tmpDir

        normalizeSet emitted `shouldBe'` normalizeSet (map toListing reference)
        let visited = Set.map dlDir (Set.fromList emitted)
        -- Loop-back symlinks must never be descended into.
        Set.member (tmpDir </> $(mkRelDir "lib/pg/lib")) visited `shouldBe'` False
        Set.member (tmpDir </> $(mkRelDir "lib/sqlite/lib")) visited `shouldBe'` False

    -- End-to-end parity on the production wiring seam: one shared
    -- 'walkStream' feeding each strategy's visitor through 'runVisitors',
    -- with per-strategy sinks and 'cachedDiscover' finalizers (mirroring
    -- "App.Fossa.Analyze.runAnalyzers", whose full effect stack cannot run in
    -- tests), versus the old per-strategy 'walkWithFilters'' path each
    -- strategy's 'discover' uses.  Cargo matches at the root and skips its
    -- whole subtree, so web/sub/Cargo.toml must stay invisible to it; Composer
    -- keeps walking, so it picks up both composer roots unless a filter prunes
    -- web/.
    it' "shared-walk wiring finds the same projects per strategy as the old per-strategy walks" $
      withTempDir "test-Discovery-WalkStream-parity" $ \tmpDir -> do
        let web = tmpDir </> $(mkRelDir "web")
            webSub = web </> $(mkRelDir "sub")
            vendor = tmpDir </> $(mkRelDir "vendor")
        sendIO $
          do
            traverse_ createDir [web, webSub, vendor]
            writeFile (toFilePath (tmpDir </> $(mkRelFile "Cargo.toml"))) "[package]"
            writeFile (toFilePath (webSub </> $(mkRelFile "Cargo.toml"))) "[package]"
            writeFile (toFilePath (web </> $(mkRelFile "composer.json"))) "{}"
            writeFile (toFilePath (web </> $(mkRelFile "composer.lock"))) "{}"
            writeFile (toFilePath (vendor </> $(mkRelFile "composer.json"))) "{}"
            writeFile (toFilePath (vendor </> $(mkRelFile "composer.lock"))) "{}"

        let noFilters = (mempty :: AllFilters)
            webExcluded = excludePath $(mkRelDir "web")

        for_ [(noFilters, 2), (webExcluded, 1)] $ \(filters, expectedComposerProjects) -> do
          -- New path: the production wiring.  One filesystem walk feeds both
          -- strategies' visitors; each accumulates into its own sink and is
          -- finalized exactly like runAnalyzers does.
          cargoSink <- sendIO $ newIORef ([] :: [Cargo.CargoProject])
          composerSink <- sendIO $ newIORef ([] :: [Composer.ComposerProject])
          let visitC dir subdirs files = accumulate cargoSink (Cargo.discoverVisitor dir subdirs files)
              visitK dir subdirs files = accumulate composerSink (Composer.discoverVisitor dir subdirs files)
          void . S.effects $ runVisitors [visitC, visitK] (walkStream filters tmpDir)
          newResult <-
            runReader filters $
              do
                cargoNew <- cachedDiscover CargoProjectType Cargo.mkProject tmpDir (sendIO $ readIORef cargoSink)
                composerNew <- cachedDiscover ComposerProjectType Composer.mkProject tmpDir (sendIO $ readIORef composerSink)
                pure (cargoNew, composerNew)
          -- Old path: each strategy walks the tree itself (walkWithFilters')
          -- under the same filters.
          oldResult <-
            runReader filters $
              do
                cargoOld <- Cargo.discover tmpDir
                composerOld <- Composer.discover tmpDir
                pure (cargoOld, composerOld)

          sort (map projectTag $ fst newResult) `shouldBe'` sort (map projectTag $ fst oldResult)
          sort (map projectTag $ snd newResult) `shouldBe'` sort (map projectTag $ snd oldResult)
          -- The comparison is not vacuous: cargo sees only the root manifest
          -- on both paths, and composer sees every filter-permitted root.
          sort (map projectTag $ fst newResult) `shouldBe'` [(show CargoProjectType, tmpDir)]
          length (snd newResult) `shouldBe'` expectedComposerProjects
          length (snd oldResult) `shouldBe'` expectedComposerProjects

-- | Mirrors the per-strategy 'prepare' of "App.Fossa.Analyze.runAnalyzers":
-- a shared-walk visitor that accumulates its strategy's raw projects into the
-- strategy's own sink and reports only its walk step, so one visitor can
-- never see another strategy's output.  A failing visitor contributes nothing
-- and keeps walking.
accumulate ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  IORef [a] ->
  m ([a], WalkStep) ->
  m ((), WalkStep)
accumulate sink action = do
  found <- warnThenRecover @Text "Error discovering projects for strategy" action
  case found of
    Nothing -> pure ((), WalkContinue)
    Just (projects, step) -> sendIO (modifyIORef' sink (<> projects)) $> ((), step)

-- | One project identified by the way its strategy sees it.
projectTag :: DiscoveredProject a -> (String, Path Abs Dir)
projectTag dp = (show (projectType dp), projectPath dp)

-- | Drain a producer into a strict list.  Uses @uncons@ so we do not need to
-- import the @Of@/@Nil@ constructors.
collect :: Stream (Of DirListing) EffectStack () -> EffectStack [DirListing]
collect str = do
  m <- S.uncons str
  case m of
    Just (a, rest) -> (a :) <$> collect rest
    Nothing -> pure []

-- | Reference: the list of @(dir, subdirs, files)@ visits performed by the
-- existing 'walk' with a consumer that always 'WalkContinue's.
collectWalk :: Path Abs Dir -> EffectStack [(Path Abs Dir, [Path Abs Dir], [Path Abs File])]
collectWalk =
  walk' (\dir subdirs files -> pure ([(dir, subdirs, files)], WalkContinue))

-- | Reference for the filtered case, running 'walkWithFilters'' under the
-- given filters.
collectWalkFiltered :: AllFilters -> Path Abs Dir -> EffectStack [(Path Abs Dir, [Path Abs Dir], [Path Abs File])]
collectWalkFiltered filters root =
  runReader filters (walkWithFilters' (\dir subdirs files -> pure ([(dir, subdirs, files)], WalkContinue)) root)

toListing :: (Path Abs Dir, [Path Abs Dir], [Path Abs File]) -> DirListing
toListing (d, s, f) = DirListing d s f

-- | Sort the (non-deterministically ordered) subdirs/files of every listing so
-- the comparison is order-independent, exactly like 'Discovery.WalkSpec'.
normalizeListing :: DirListing -> DirListing
normalizeListing dl = dl{dlSubdirs = sort (dlSubdirs dl), dlFiles = sort (dlFiles dl)}

normalizeSet :: [DirListing] -> Set DirListing
normalizeSet = Set.fromList . map normalizeListing
